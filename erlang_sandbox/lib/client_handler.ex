defmodule ErlangSandbox.ClientHandler do
  require Logger
  import ErlangSandbox.Erlang.Loader, only: [handle_erlang_code: 1]
  import ErlangSandbox.Tests, only: [run_tests: 2]

  def handle_request({:ok,%{"op" => "compile", "code" => code}}) do
    Logger.info("Executing Erlang handler")
    case handle_erlang_code(code) do
      {:ok, response} -> {:ok, response}
      {:error, reason} -> {:error, reason}
    end
  end

  def handle_request({:ok,%{"op" => "test", "code" => code, "cases" => cases}}) do
    Logger.info("Executing Erlang test cases")
    case run_tests(code, cases) do
      {:ok, output, results} -> {:ok, output, results}
      {:error, reason} -> {:error, reason}
    end
  end

  def handle_request({:ok,_res}) do
    {:error, "Payload malformed"}
  end
end
