defmodule ErlangSandbox.ConnectionManager do
  use GenServer
  use AMQP
  require Logger

  def start_link(broker_port) do
    GenServer.start_link(__MODULE__, broker_port, name: __MODULE__)
  end

  def get_channel do
    GenServer.call(__MODULE__, :get_channel)
  end

  def init(broker_port) do
    send(self(), :connect)
    {:ok, %{
      broker_port: broker_port,
      connection: nil,
      channels: [],
      retry_count: 0
      }}
  end

  def handle_info(:connect, %{broker_port: port, retry_count: count} = state) do
    case connect_to_rabbitmq(port) do
      {:ok, conn} ->
        Process.monitor(conn.pid)
        Logger.info("Connected to RabbitMQ")
        {:noreply, %{state | connection: conn, retry_count: 0}}

      {:error, reason} ->
        Logger.error("Failed to connect: #{inspect(reason)}, retry #{count + 1}")
        backoff = min(1000 * :math.pow(2, count), 30_000) |> trunc()
        Process.send_after(self(), :connect, backoff)
        {:noreply, %{state | retry_count: count + 1}}
    end
  end

  def handle_info({:DOWN, _ref, :process, _pid, reason}, state) do
    Logger.error("RabbitMQ connection lost: #{inspect(reason)}")
    send(self(), :connect)
    {:noreply, %{state | connection: nil, channels: []}}
  end

  def handle_call(:get_channel, _from, %{connection: nil} = state) do
    {:reply, {:error, :not_connected}, state}
  end

  def handle_call(:get_channel, _from, %{connection: conn, channels: channels} = state) do
    case Channel.open(conn) do
      {:ok, chan} ->
        Process.monitor(chan.pid)
        {:reply, {:ok, chan}, %{state | channels: [chan | channels]}}

      {:error, reason} ->
        {:reply, {:error, reason}, state}
    end
  end

  defp connect_to_rabbitmq(broker_port) do
    host = Application.get_env(:erlang_sandbox, :host)
    user = Application.get_env(:erlang_sandbox, :user)
    password = Application.get_env(:erlang_sandbox, :password)

    Connection.open("amqp://#{user}:#{password}@#{host}:#{broker_port}")
  end

  def terminate(_reason, %{connection: conn, channels: channels}) do
    Enum.each(channels, &Channel.close/1)
    if conn, do: Connection.close(conn)
    :ok
  end
end
