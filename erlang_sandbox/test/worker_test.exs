defmodule ErlangSandbox.WorkerTest do
  use ExUnit.Case

  setup do
    :meck.new(AMQP.Basic, [:non_strict])
    :meck.new(AMQP.Queue, [:non_strict])
    :meck.new(ErlangSandbox.ConnectionManager, [:non_strict])
    :meck.new(ErlangSandbox.RequestHandler, [:non_strict])

    on_exit(fn ->
      :meck.unload()
    end)

    :ok
  end

  test "setup_channel connects and starts consuming" do
    chan = %{pid: self()}

    :meck.expect(ErlangSandbox.ConnectionManager, :get_channel, fn ->
      {:ok, chan}
    end)

    :meck.expect(AMQP.Queue, :declare, fn _chan, _queue, _opts ->
      {:ok, :queue_declared}
    end)

    :meck.expect(AMQP.Basic, :qos, fn _chan, _opts -> :ok end)
    :meck.expect(AMQP.Basic, :consume, fn _chan, _queue -> {:ok, :consumer_tag} end)

    {:ok, pid} = ErlangSandbox.Worker.start_link([])

    assert wait_for_channel(pid, chan)
  end

  test "basic_deliver publishes response and acks message" do
    test_pid = self()
    chan = %{pid: self()}

    :meck.expect(ErlangSandbox.ConnectionManager, :get_channel, fn ->
      {:ok, chan}
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

    {:ok, worker_pid} = ErlangSandbox.Worker.start_link([])
    assert wait_for_channel(worker_pid, chan)

    payload = Jason.encode!(%{"op" => "compile", "code" => "ok"})
    meta = %{delivery_tag: 123, reply_to: "reply_queue", correlation_id: "corr-id-1"}

    send(worker_pid, {:basic_deliver, payload, meta})

    assert_receive {:published, "reply_queue", published_payload, [correlation_id: "corr-id-1"]}, 1000

    decoded = Jason.decode!(published_payload)
    assert decoded["status"] == "ok"
    assert decoded["result"] == "worker_ok"

    assert_receive {:acked, 123}, 1000
  end

  require ErlangSandbox.Worker
  test "public api is exposed" do
    assert function_exported?(ErlangSandbox.Worker, :start_link, 1)
    assert function_exported?(ErlangSandbox.Worker, :child_spec, 1)
  end

  defp wait_for_channel(pid, chan, attempts \\ 20)
  defp wait_for_channel(_pid, _chan, 0), do: false

  defp wait_for_channel(pid, chan, attempts) do
    case :sys.get_state(pid) do
      %{channel: ^chan} ->
        true

      _ ->
        Process.sleep(10)
        wait_for_channel(pid, chan, attempts - 1)
    end
  end
end
