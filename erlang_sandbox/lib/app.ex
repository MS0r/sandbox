defmodule ErlangSandbox.Application do
  use Application
  require Logger

  def start(_type, _args) do
    case Application.get_env(:erlang_sandbox, :env) do
      :test ->
        # In test environment, just return the current process
        Logger.info("Testing the service...")
        {:ok, self()}

      env ->
        # Read the configured port for dev or prod
        broker_port = Application.get_env(:erlang_sandbox, :port)
        Logger.info("Starting the Sandbox service in #{env} environment")

        # Define children for the supervision tree
        children = [
          {ErlangSandbox.ConnectionManager, broker_port},
          ErlangSandbox.QueueManager,
          {ErlangSandbox.Worker, id: :worker_1},
          {ErlangSandbox.Worker, id: :worker_2},
          {ErlangSandbox.Worker, id: :worker_3}
        ]

        # Supervision options
        opts = [strategy: :rest_for_one, name: ErlangSandbox.Supervisor]
        Supervisor.start_link(children, opts)
    end
  end
end
