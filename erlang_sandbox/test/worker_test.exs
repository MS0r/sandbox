defmodule ErlangSandbox.WorkerTest do
  use ExUnit.Case, async: false

  setup do
    :meck.new(AMQP.Basic, [:non_strict])
    :meck.new(AMQP.Queue, [:non_strict])
    :meck.new(AMQP.Channel, [:non_strict])
    :meck.new(ErlangSandbox.ConnectionManager, [:non_strict])
    :meck.new(ErlangSandbox.QueueManager, [:non_strict])
    :meck.new(ErlangSandbox.RequestHandler, [:non_strict])

    # Mock Channel.close globally for all tests
    :meck.expect(AMQP.Channel, :close, fn _chan -> :ok end)

    on_exit(fn ->
      # Give processes time to terminate before unmocking
      Process.sleep(50)

      # Clean up registered process if it exists
      case Process.whereis(ErlangSandbox.ConnectionManager) do
        nil -> :ok
        pid when is_pid(pid) ->
          Process.unregister(ErlangSandbox.ConnectionManager)
      end

      :meck.unload()
    end)

    :ok
  end

  test "setup_channel connects and starts consuming" do
    chan = %{pid: self()}
    con_pid = spawn(fn ->
      receive do
        :stop -> :ok
      end
    end)
    Process.register(con_pid, ErlangSandbox.ConnectionManager)

    :meck.expect(ErlangSandbox.ConnectionManager, :get_channel, fn ->
      {:ok, chan}
    end)

    :meck.expect(ErlangSandbox.QueueManager, :wait_for_setup, fn _timeout ->
      :ok
    end)

    :meck.expect(AMQP.Queue, :declare, fn _chan, _queue, _opts ->
      {:ok, :queue_declared}
    end)

    :meck.expect(AMQP.Basic, :qos, fn _chan, _opts -> :ok end)
    :meck.expect(AMQP.Basic, :consume, fn _chan, _queue -> {:ok, :consumer_tag} end)

    {:ok, pid} = ErlangSandbox.Worker.start_link([id: :test_worker])

    assert wait_for_channel(pid, chan, :test_worker)

    # Stop worker before ending test
    GenServer.stop(pid, :normal)
    send(con_pid, :stop)
  end

  test "basic_deliver publishes response and acks message" do
    test_pid = self()
    chan = %{pid: self()}
    con_pid = spawn(fn ->
      receive do
        :stop -> :ok
      end
    end)
    Process.register(con_pid, ErlangSandbox.ConnectionManager)

    :meck.expect(ErlangSandbox.ConnectionManager, :get_channel, fn ->
      {:ok, chan}
    end)

    :meck.expect(ErlangSandbox.QueueManager, :wait_for_setup, fn _timeout ->
      :ok
    end)

    :meck.expect(AMQP.Queue, :declare, fn _chan, _queue, _opts ->
      {:ok, :queue_declared}
    end)

    :meck.expect(AMQP.Basic, :qos, fn _chan, _opts -> :ok end)
    :meck.expect(AMQP.Basic, :consume, fn _chan, _queue -> {:ok, :consumer_tag} end)

    :meck.expect(ErlangSandbox.RequestHandler, :handle_request, fn {:ok, _payload} ->
      {:ok, "worker_ok"}
    end)

    :meck.expect(AMQP.Basic, :publish, fn _chan, _exchange, routing_key, payload, opts ->
      send(test_pid, {:published, routing_key, payload, opts})
      :ok
    end)

    :meck.expect(AMQP.Basic, :ack, fn _chan, delivery_tag ->
      send(test_pid, {:acked, delivery_tag})
      :ok
    end)

    {:ok, worker_pid} = ErlangSandbox.Worker.start_link([id: :test_worker])
    assert wait_for_channel(worker_pid, chan, :test_worker)

    payload = Jason.encode!(%{"op" => "compile", "code" => "ok"})
    meta = %{delivery_tag: 123, reply_to: "reply_queue", correlation_id: "corr-id-1", worker_id: :test_worker}

    send(worker_pid, {:basic_deliver, payload, meta})

    assert_receive {:published, "reply_queue", published_payload, [correlation_id: "corr-id-1", content_type: "application/json"]}, 1000

    decoded = Jason.decode!(published_payload)
    assert decoded["status"] == "ok"
    assert decoded["result"] == "worker_ok"

    assert_receive {:acked, 123}, 1000

    # Stop worker before ending test
    GenServer.stop(worker_pid, :normal)
    send(con_pid, :stop)
  end

  require ErlangSandbox.Worker
  test "public api is exposed" do
    assert function_exported?(ErlangSandbox.Worker, :start_link, 1)
    assert function_exported?(ErlangSandbox.Worker, :child_spec, 1)
  end

  defp wait_for_channel(pid, chan, id, attempts \\ 20)
  defp wait_for_channel(_pid, _chan, _id, 0), do: false

  defp wait_for_channel(pid, chan, id, attempts) do
    case :sys.get_state(pid) do
      %{worker_id: ^id, channel: ^chan, retry_count: 0, messages_processed: 0} ->
        true

      _ ->
        Process.sleep(10)
        wait_for_channel(pid, chan, id, attempts - 1)
    end
  end
end
