defmodule ErlangSandbox.QueueManager do
  use GenServer
  use AMQP
  require Logger

  @queue "rpc_queue"
  @queue_error "#{@queue}_error"

  def start_link(_opts) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def wait_for_setup(timeout \\ 30_000) do
    GenServer.call(__MODULE__, :wait_for_setup, timeout)
  end

  def init(:ok) do
    send(self(), :setup_queues)
    {:ok, %{setup_complete: false, retry_count: 0}}
  end

  def handle_info(:setup_queues, %{retry_count: count} = state) do
    case ErlangSandbox.ConnectionManager.get_channel() do
      {:ok, chan} ->
        try do
          setup_queues(chan)
          Channel.close(chan)
          Logger.info("✓ Queues successfully set up")
          {:noreply, %{state | setup_complete: true, retry_count: 0}}
        rescue
          e ->
            Logger.error("Failed to setup queues: #{Exception.message(e)}")
            retry_setup(count, state)
        end

      {:error, reason} ->
        Logger.warning("Failed to get channel for queue setup (attempt #{count + 1}): #{inspect(reason)}")
        retry_setup(count, state)
    end
  end

  def handle_call(:wait_for_setup, _from, %{setup_complete: true} = state) do
    {:reply, :ok, state}
  end

  def handle_call(:wait_for_setup, _from, %{setup_complete: false}) do
    {:error, "Still on process..."}
  end

  defp retry_setup(count, state) do
    backoff = min(1000 * :math.pow(2, count), 10_000) |> trunc()
    Logger.info("Retrying queue setup in #{backoff}ms...")
    Process.send_after(self(), :setup_queues, backoff)
    {:noreply, %{state | retry_count: count + 1}}
  end

  defp setup_queues(chan) do
    Logger.debug("Setting up RPC queues...")
    {:ok, _} = Queue.declare(chan, @queue_error, durable: true)

    {:ok, _} = Queue.declare(chan, @queue,
      durable: true,
      arguments: [
        {"x-dead-letter-exchange", :longstr, ""},
        {"x-dead-letter-routing-key", :longstr, @queue_error}
      ]
    )
  end
end
