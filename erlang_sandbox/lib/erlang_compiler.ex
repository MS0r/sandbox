defmodule ErlangSandbox.Erlang do
  defmodule Parser do
    def parse_all_forms(tokens), do: parse_forms(tokens, [])

    defp parse_forms([], acc), do: {:ok, Enum.reverse(acc)}

    defp parse_forms(tokens, acc) do
      case split_next_form(tokens) do
        {:ok, form_tokens, rest} ->
          case :erl_parse.parse_form(form_tokens) do
            {:ok, form} ->
              parse_forms(rest, [form | acc])

            {:error, err} ->
              {:error, err}
          end

        {:error, :unexpected_end_of_input, bad_tokens} ->
          {:error, {:unexpected_end_of_input, bad_tokens}}

        {:error, _} = err ->
          err
      end
    end

    defp split_next_form(tokens), do: do_split_next_form(tokens, [])

    defp do_split_next_form([], acc), do: {:error, :unexpected_end_of_input, acc}

    defp do_split_next_form([{:dot, _} = dot | rest], acc) do
      {:ok, Enum.reverse([dot | acc]), rest}
    end

    defp do_split_next_form([head | tail], acc) do
      do_split_next_form(tail, [head | acc])
    end
  end

  defmodule Loader do
    import ErlangSandbox.Erlang.Parser, only: [parse_all_forms: 1]

    def compile_and_load_erlang_module(source_code) do
      charlist = to_charlist(String.trim(source_code))

      with {:ok, tokens, _} <- :erl_scan.string(charlist),
           {:ok, forms} <- parse_all_forms(tokens) do
        case :compile.forms(forms, [:binary]) do
          {:ok, module, binary} ->
            case :code.load_binary(module, ~c'', binary) do
              {:module, ^module} -> {:ok, module}
              err -> {:error, err}
            end

          {:error, reason} ->
            {:error, reason}
        end
      else
        {:error, reason, _} ->
          {:error, reason}

        {:error, _} = err ->
          err

        other ->
          {:error, other}
      end
    end

    def run_module({atom, module}) do
      {:ok, io} = StringIO.open("")
      previous_leader = Process.group_leader()

      case atom do
        :run ->
          owner = spawn(fn -> Process.sleep(:infinity) end)
          Process.group_leader(owner, io)

          {pid, _spawns} =
            spawn_monitor(fn ->
              Process.group_leader(self(), io)
              apply(module, :start, [])
            end)

          receive do
            {:DOWN, _ref, :process, ^pid, reason} ->
              case reason do
                :normal ->
                  :ok

                {reason, stacktrace} ->
                  msg = :erl_error.format_exception(:error, reason, stacktrace)
                  raise RuntimeError, message: to_string(msg)

                other ->
                  raise RuntimeError, other
              end
          after
            1000 -> raise RuntimeError, message: "TimeoutError: execution exceeded 1000ms"
          end

          Process.exit(owner, :kill)
          Process.group_leader(self(), previous_leader)
          StringIO.contents(io)

        :test ->
          Process.group_leader(self(), io)
          test_results = ExUnit.run()

          {_, output} = StringIO.contents(io)
          Process.group_leader(self(), previous_leader)
          {:ok, output, test_results}
      end
    end

    def handle_erlang_code(source_code) do
      case compile_and_load_erlang_module(source_code) do
        {:ok, module} ->
          try do
            {_, output} = run_module({:run, module})
            {:ok, output}
          rescue
            e in RuntimeError -> {:error, e.message}
          end

        {:error, reason} ->
          {:error, reason}
      end
    end
  end
end
