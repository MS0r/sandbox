defmodule ErlangSandbox.Tests do
  import ErlangSandbox.Erlang.Loader, only: [compile_and_load_erlang_module: 1, run_module: 1]

  def run_tests(source_code, cases) do
    case compile_and_load_erlang_module(source_code) do
      {:ok, _module} ->
        ExUnit.start()

        try do
          case Code.compile_string(cases) do
            [{mod, _bin} | _] ->
              run_module({:test, mod})

            [] ->
              {:error, "No module compiled from test cases"}
          end
        rescue
          e in RuntimeError -> {:error, e.message}
          e in CompileError -> {:error, e.description}
        end

      {:error, reason} ->
        {:error, reason}
    end
  end
end
