defmodule ErlangSandbox.Worker do
  use GenServer
  use AMQP
  import ErlangSandbox.HandleError, only: [to_string_response: 1]
  import ErlangSandbox.ClientHandler, only: [handle_request: 1]

  def start_link(broker_port \\ 5672) do
    GenServer.start_link(__MODULE__, {broker_port}, name: __MODULE__)
  end

  @queue       "rpc_queue"
  @queue_error "#{@queue}_error"

  def init({broker_port}) do
    host = System.get_env("RABBITMQ_HOST")
    user = System.get_env("RABBITMQ_DEFAULT_USER")
    password = System.get_env("RABBITMQ_DEFAULT_PASS")

    {:ok, conn} = Connection.open("amqp://#{user}:#{password}@#{host}:#{broker_port}")
    {:ok, chan} = Channel.open(conn)
    setup_queue(chan)

    Basic.qos(chan, prefetch_count: 3)
    Basic.consume(chan, @queue)
    wait_for_messages(chan)
  end

  def wait_for_messages(chan) do
    receive do
      {:basic_deliver, payload, meta} ->
        Task.start(fn -> handle_client(chan,meta,payload) end)
        wait_for_messages(chan)
    end
  end

  defp handle_client(chan, meta, payload) do
    case handle_request(Jason.decode(payload)) do
      {:ok, response} ->
        send_json(chan, meta, %{status: "ok", result: to_string_response({:ok, response})})

      {:error, reason} ->
        send_json(chan, meta, %{status: "error", reason: to_string_response({:error, reason})})

      {:ok, output, test_results} ->
        send_json(chan, meta, %{
          status: "ok",
          result: to_string_response({:ok, output}),
          test_results: test_results
        })
      end
    Basic.ack(chan,meta.delivery_tag)
  end

  defp send_json(chan,meta,data) do
    json = Jason.encode!(data)
    Basic.publish(chan,
                       "",
                       meta.reply_to,
                       "#{json}",
                       correlation_id: meta.correlation_id
    )
  end

  def setup_queue(chan) do
    {:ok, _} = Queue.declare(chan,@queue_error, durable: true)

    {:ok, _} = Queue.declare(chan, @queue,
                             durable: true,
                             arguments: [
                                {"x-dead-letter-exchange", :longstr, ""},
                                {"x-dead-letter-routing-key", :longstr, @queue_error}
                             ]
                            )
  end
end
