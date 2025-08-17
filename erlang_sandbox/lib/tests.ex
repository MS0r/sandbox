defmodule ErlangSandbox.Tests do
  import ErlangSandbox.Erlang.Loader, only: [compile_and_load_erlang_module: 1, run_module: 1]

  def run_tests(source_code, cases) do
    case compile_and_load_erlang_module(source_code) do
      {:ok, _module} ->
        ExUnit.start()
        case Code.compile_string(cases) do
          [{mod, _bin} | _] ->
            try do
              run_module({:test, mod})
            rescue
              e in RuntimeError -> {:error, e.message}
            end
          [] ->
            {:error, "No module compiled from test cases"}

          other ->
            {:error, other}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end
end
