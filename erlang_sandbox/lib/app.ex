defmodule ErlangSandbox.Application do
  use Application

  def start(_type, _args) do
    case Application.get_env(:erlang_sandbox, :env, :dev) do
      :test ->
        # In test environment, just return the current process
        {:ok, self()}

      _ ->
        # Read the configured port for dev or prod
        port = Application.get_env(:erlang_sandbox, :port, 4000)

        # Define children for the supervision tree
        children = [
          {ErlangSandbox.Server, port}
        ]

        # Supervision options
        opts = [strategy: :one_for_one, name: ErlangSandbox.Supervisor]
        Supervisor.start_link(children, opts)
    end
  end
end
