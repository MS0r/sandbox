defmodule ErlangSandbox.HealthServer do
  use Plug.Router
  require Logger

  plug :match
  plug :dispatch

  @health_port 4000

  def start_link(_opts) do
    Logger.info("Starting health server on port #{@health_port}")
    Bandit.start_link(plug: __MODULE__, port: @health_port)
  end

  def child_spec(opts) do
    %{
      id: __MODULE__,
      start: {__MODULE__, :start_link, [opts]},
      type: :worker,
      restart: :permanent
    }
  end

  get "/health" do
    health_status = check_health()
    
    status_code = if health_status.status == "healthy", do: 200, else: 503
    
    conn
    |> put_resp_content_type("application/json")
    |> send_resp(status_code, Jason.encode!(health_status))
  end

  match _ do
    send_resp(conn, 404, "Not Found")
  end

  defp check_health do
    rabbitmq_status = check_rabbitmq_connection()
    
    overall_status = if rabbitmq_status == :up, do: "healthy", else: "unhealthy"
    
    %{
      status: overall_status,
      connection: rabbitmq_status,
      timestamp: DateTime.utc_now() |> DateTime.to_iso8601()
    }
  end

  defp check_rabbitmq_connection do
    case Process.whereis(ErlangSandbox.ConnectionManager) do
      nil -> 
        :down
      _pid ->
        case ErlangSandbox.ConnectionManager.get_channel() do
          {:ok, chan} -> 
            AMQP.Channel.close(chan)
            :up
          {:error, _} -> 
            :down
        end
    end
  end
end
