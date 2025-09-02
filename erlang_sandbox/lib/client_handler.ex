defmodule ErlangSandbox.ClientHandler do
  import ErlangSandbox.Erlang.Loader, only: [handle_erlang_code: 1]
  import ErlangSandbox.Tests, only: [run_tests: 2]

  def handle_request({:ok,%{"op" => "compile", "code" => code}}) do
    case handle_erlang_code(code) do
      {:ok, response} -> {:ok, response}
      {:error, reason} -> {:error, reason}
    end
  end

  def handle_request({:ok,%{"op" => "test", "code" => code, "cases" => cases}}) do
    case run_tests(code, cases) do
      {:ok, output, results} -> {:ok, output, results}
      {:error, reason} -> {:error, reason}
    end
  end
end
