defmodule ErlangSandbox.WorkerTest do
  use ExUnit.Case

  setup do
    :meck.new(AMQP.Basic, [:non_strict])
    :meck.new(ErlangSandbox.ClientHandler, [:non_strict])

    on_exit(fn ->
      :meck.unload()
    end)

    :ok
  end

  test "wait_for_messages delivers response and acks message" do
    test_pid = self()

    :meck.expect(ErlangSandbox.ClientHandler, :handle_request, fn {:ok, _payload} ->
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

    worker_pid = spawn(fn -> ErlangSandbox.Worker.wait_for_messages(:fake_chan) end)

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
    assert function_exported?(ErlangSandbox.Worker, :wait_for_messages, 1)
    assert function_exported?(ErlangSandbox.Worker, :setup_queue, 1)
  end
end
