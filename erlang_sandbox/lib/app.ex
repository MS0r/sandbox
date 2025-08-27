defmodule ErlangSandbox.Application do
  use Application

  def start(_type, _args) do

    if Mix.env() == :test do
      # In test environment, we don't start the server to avoid port conflicts
      {:ok, self()}
    else
      # Start the server in non-test environments
      ErlangSandbox.Server.start()
    end
    # Return a dummy supervisor or `:ignore` if no supervision
  end
end
