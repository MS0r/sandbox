defmodule ErlangSandbox.Worker do
  use GenServer
  use AMQP
  require Logger
  import ErlangSandbox.HandleError, only: [to_string_response: 1]
  import ErlangSandbox.RequestHandler, only: [handle_request: 1]

  @queue "rpc_queue"
  @queue_error "#{@queue}_error"
  @prefetch 2
  
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts)
  end

  def init(_opts) do
    send(self(), :setup_channel)
    {:ok, %{channel: nil}}
  end

  def child_spec(opts) do
    worker_id = Keyword.fetch!(opts, :id)

    %{
      id: worker_id,
      start: {__MODULE__, :start_link, [opts]},
      restart: :permanent,
      shutdown: 5_000,
      type: :worker
    }
  end

  def handle_info(:setup_channel, state) do
    case ErlangSandbox.ConnectionManager.get_channel() do
      {:ok, chan} ->
        Process.monitor(chan.pid)
        setup_queue(chan)
        Basic.qos(chan, prefetch_count: @prefetch)
        {:ok, _} = Basic.consume(chan, @queue)
        Logger.info("Worker ready with channel")
        {:noreply, %{state | channel: chan}}

      {:error, reason} ->
        Logger.error("Failed to get channel: #{inspect(reason)}, retrying...")
        Process.send_after(self(), :setup_channel, 5_000)
        {:noreply, state}
    end
  end

  def handle_info({:DOWN, _ref, :process, _pid, reason}, state) do
    Logger.error("Channel lost: #{inspect(reason)}, reconnecting...")
    send(self(), :setup_channel)
    {:noreply, %{state | channel: nil}}
  end

  def handle_info({:basic_consume_ok, _}, state), do: {:noreply, state}
  def handle_info({:basic_cancel, _}, state), do: {:stop, :normal, state}
  def handle_info({:basic_cancel_ok, _}, state), do: {:noreply, state}

  def handle_info({:basic_deliver, payload, meta}, %{channel: chan} = state) do
    Logger.debug("Message received from #{@queue}")

    handle_client(chan, meta, payload)

    {:noreply, state}
  end

  defp handle_client(chan, meta, payload) do
    try do
      Logger.metadata(correlation_id: meta.correlation_id)

      case handle_request(Jason.decode(payload)) do
        {:ok, response} ->
          Logger.info("Successful execution of request")
          send_json(chan, meta, %{status: "ok", result: to_string_response({:ok, response})})
          Basic.ack(chan, meta.delivery_tag)

        {:error, reason} ->
          res = to_string_response({:error, reason})
          Logger.error("Error: #{res}")
          send_json(chan, meta, %{status: "error", reason: res})
          Basic.ack(chan, meta.delivery_tag)

        {:ok, output, test_results} ->
          Logger.info("Successful testing of request")
          send_json(chan, meta, %{
            status: "ok",
            result: to_string_response({:ok, output}),
            test_results: test_results
          })
          Basic.ack(chan, meta.delivery_tag)
      end
    rescue
      e ->
        Logger.error("Unhandled error: #{Exception.message(e)}")
        Basic.reject(chan, meta.delivery_tag, requeue: false)
    end
  end

  defp send_json(chan, meta, data) do
    json = Jason.encode!(data)
    Logger.info("Publishing response to queue #{meta.reply_to}")
    Basic.publish(chan, "", meta.reply_to, json, correlation_id: meta.correlation_id)
  end

  defp setup_queue(chan) do
    Logger.debug("Setting up rpc_queues")
    {:ok, _} = Queue.declare(chan, @queue_error, durable: true)

    {:ok, _} = Queue.declare(chan, @queue,
      durable: true,
      arguments: [
        {"x-dead-letter-exchange", :longstr, ""},
        {"x-dead-letter-routing-key", :longstr, @queue_error}
      ]
    )
  end

  def terminate(_reason, %{channel: chan}) do
    if chan, do: Channel.close(chan)
    :ok
  end
end
