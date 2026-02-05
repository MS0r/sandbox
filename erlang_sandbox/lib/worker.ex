defmodule ErlangSandbox.Worker do
  use GenServer
  use AMQP
  require Logger
  import ErlangSandbox.HandleError, only: [to_string_response: 1]
  import ErlangSandbox.ClientHandler, only: [handle_request: 1]

  def start_link(broker_port) do
    GenServer.start_link(__MODULE__, {broker_port}, name: __MODULE__)
  end

  @queue "rpc_queue"
  @queue_error "#{@queue}_error"

  def init({broker_port}) do
    host = Application.get_env(:erlang_sandbox, :host)
    user = Application.get_env(:erlang_sandbox, :user)
    password = Application.get_env(:erlang_sandbox, :password)

    Logger.debug("Connecting to RabbitMQ instance in host #{host} and port #{broker_port}")
    {:ok, conn} = Connection.open("amqp://#{user}:#{password}@#{host}:#{broker_port}")
    Logger.debug("Succesful connection to RabbitMQ")
    {:ok, chan} = Channel.open(conn)
    setup_queue(chan)

    Basic.qos(chan, prefetch_count: 3)
    Basic.consume(chan, @queue)
    Logger.debug("Set basic consume on queue #{@queue} with prefetch 3")
    Logger.debug("Successfully initialize service. Waiting for messages...")
    wait_for_messages(chan)
  end

  def wait_for_messages(chan) do
    receive do
      {:basic_deliver, payload, meta} ->
        Logger.debug("Message received from #{@queue}", metadata: meta)
        Task.start(fn -> handle_client(chan,meta,payload) end)
        wait_for_messages(chan)
    end
  end

  defp handle_client(chan, meta, payload) do
    Logger.metadata(meta)
    case handle_request(Jason.decode(payload)) do
      {:ok, response} ->
        Logger.info("Successful execution of request.")
        send_json(chan, meta, %{status: "ok", result: to_string_response({:ok, response})})
      {:error, reason} ->
        res = to_string_response({:error, reason})
        Logger.error("Error: #{res}.")
        send_json(chan, meta, %{status: "error", reason: res})

      {:ok, output, test_results} ->
        Logger.info("Successful testing of request.")
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
    Logger.info("Publishing response to queue #{meta.reply_to}")
    Basic.publish(chan,
                       "",
                       meta.reply_to,
                       "#{json}",
                       correlation_id: meta.correlation_id
    )
  end

  def setup_queue(chan) do
    Logger.debug("Setting up rpc_queues")
    {:ok, _} = Queue.declare(chan,@queue_error, durable: true)

    {:ok, _} = Queue.declare(chan, @queue,
                             durable: true,
                             arguments: [
                                {"x-dead-letter-exchange", :longstr, ""},
                                {"x-dead-letter-routing-key", :longstr, @queue_error}
                             ]
                            )
    Logger.debug("Queues successfully set up")
  end
end
