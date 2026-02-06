defmodule ErlangSandbox.Worker do
  use GenServer
  use AMQP
  require Logger
  import ErlangSandbox.HandleError, only: [to_string_response: 1]
  import ErlangSandbox.RequestHandler, only: [handle_request: 1]

  @queue "rpc_queue"
  @prefetch 2
  @retry_interval 2_000
  @max_retry_interval 30_000

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts)
  end

  def init(opts) do
    worker_id = Keyword.get(opts, :id, :unknown)
    Process.flag(:trap_exit, true)
    send(self(), :setup_channel)

    {:ok,
     %{
       worker_id: worker_id,
       channel: nil,
       retry_count: 0,
       messages_processed: 0
     }}
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

  def handle_info(:setup_channel, %{retry_count: count} = state) do
    case ErlangSandbox.QueueManager.wait_for_setup(5_000) do
      :ok ->
        setup_worker_channel(count, state)

      {:error, reason} ->
        Logger.error("Queue setup not ready: #{inspect(reason)}")
        backoff = calculate_backoff(count)
        Process.send_after(self(), :setup_channel, backoff)
        {:noreply, %{state | retry_count: count + 1}}
    end
  catch
    :exit, reason ->
      Logger.error("QueueManager not available: #{inspect(reason)}")
      backoff = calculate_backoff(count)
      Process.send_after(self(), :setup_channel, backoff)
      {:noreply, %{state | retry_count: count + 1}}
  end

  def handle_info({:DOWN, _ref, :process, _pid, reason}, state) do
    Logger.error("Worker #{state.worker_id} channel lost: #{inspect(reason)}")
    send(self(), :setup_channel)
    {:noreply, %{state | channel: nil, retry_count: 0}}
  end

  def handle_info({:basic_consume_ok, %{consumer_tag: tag}}, state) do
    Logger.debug("Worker #{state.worker_id} consuming with tag: #{tag}")
    {:noreply, state}
  end

  def handle_info({:basic_cancel, _}, state), do: {:stop, :normal, state}
  def handle_info({:basic_cancel_ok, _}, state), do: {:noreply, state}

  def handle_info({:basic_deliver, payload, meta}, %{channel: chan} = state) do
    start_time = System.monotonic_time(:millisecond)

    Logger.metadata(
      worker_id: state.worker_id,
      correlation_id: meta.correlation_id
    )
    Logger.debug("Message received")

    handle_client(chan, meta, payload)

    duration = System.monotonic_time(:millisecond) - start_time
    Logger.debug("Message processed in #{duration}ms")

    {:noreply, %{state | messages_processed: state.messages_processed + 1}}
  end

  def handle_call(:stats, _from, state) do
    stats = %{
      worker_id: state.worker_id,
      channel_open: state.channel != nil,
      messages_processed: state.messages_processed,
      retry_count: state.retry_count
    }

    {:reply, stats, state}
  end

  defp get_channel_safe do
    try do
      case Process.whereis(ErlangSandbox.ConnectionManager) do
        nil -> {:error, :connection_manager_not_started}
        _pid -> ErlangSandbox.ConnectionManager.get_channel()
      end
    catch
      :exit, reason -> {:error, {:exit, reason}}
    end
  end

  defp setup_worker_channel(count, state) do
    case get_channel_safe() do
      {:ok, chan} ->
        try do
          Process.monitor(chan.pid)
          Basic.qos(chan, prefetch_count: @prefetch)
          {:ok, _consumer_tag} = Basic.consume(chan, @queue)

          Logger.info("Worker #{state.worker_id} ready with channel")
          {:noreply, %{state | channel: chan, retry_count: 0}}
        rescue
          e ->
            Logger.error("Worker #{state.worker_id} failed to setup: #{Exception.message(e)}")
            Channel.close(chan)
            backoff = calculate_backoff(count)
            Process.send_after(self(), :setup_channel, backoff)
            {:noreply, %{state | retry_count: count + 1}}
        end

      {:error, reason} ->
        backoff = calculate_backoff(count)
        Logger.warning(
          "Worker #{state.worker_id} failed to get channel (attempt #{count + 1}): " <>
          "#{inspect(reason)}, retrying in #{backoff}ms"
        )

        Process.send_after(self(), :setup_channel, backoff)
        {:noreply, %{state | retry_count: count + 1}}
    end
  end

  defp calculate_backoff(retry_count) do
    min(@retry_interval * :math.pow(2, retry_count), @max_retry_interval)
    |> trunc()
  end

  defp handle_client(chan, meta, payload) do
    try do
      case handle_request(Jason.decode(payload)) do
        {:ok, response} ->
          Logger.info("Successful execution")
          send_json(chan, meta, %{status: "ok", result: to_string_response({:ok, response})})
          Basic.ack(chan, meta.delivery_tag)

        {:error, reason} ->
          res = to_string_response({:error, reason})
          Logger.error("Error: #{res}")
          send_json(chan, meta, %{status: "error", reason: res})
          Basic.ack(chan, meta.delivery_tag)

        {:ok, output, test_results} ->
          Logger.info("Successful testing")

          send_json(chan, meta, %{
            status: "ok",
            result: to_string_response({:ok, output}),
            test_results: test_results
          })

          Basic.ack(chan, meta.delivery_tag)
      end
    rescue
      e in Jason.DecodeError ->
        Logger.error("JSON decode error: #{Exception.message(e)}")
        send_json(chan, meta, %{status: "error", reason: "Invalid JSON payload"})
        Basic.reject(chan, meta.delivery_tag, requeue: false)

      e ->
        Logger.error("Unhandled error: #{Exception.message(e)}\n#{Exception.format_stacktrace()}")
        send_json(chan, meta, %{status: "error", reason: "Internal server error"})
        Basic.reject(chan, meta.delivery_tag, requeue: false)
    end
  end

  defp send_json(chan, meta, data) do
    json = Jason.encode!(data)

    Basic.publish(
      chan,
      "",
      meta.reply_to,
      json,
      correlation_id: meta.correlation_id,
      content_type: "application/json"
    )
  end

  def terminate(reason, state) do
    Logger.info(
      "Worker #{state.worker_id} terminating: #{inspect(reason)}, processed #{state.messages_processed} messages"
    )

    if state.channel, do: Channel.close(state.channel)
    :ok
  end
end
