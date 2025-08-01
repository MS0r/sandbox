defmodule ErlangSandbox.Application do
  use Application

  def start(_type, _args) do
    ErlangSandbox.Server.start()
    # Return a dummy supervisor or `:ignore` if no supervision
    {:ok, self()}
  end
end
