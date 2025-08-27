defmodule ErlangSandbox.HandleError do
  def to_string_response({:ok, msg}), do: "#{msg}"

  def to_string_response({:error, {:unexpected_end_of_input, [{type,location} | _tail]}}) do
    "ERROR: unexpected end of input on line #{location} with value '#{type}' \n"
  end

  def to_string_response({:error, {:unexpected_end_of_input, [{type,location,value} | _tail]}}) do
    "ERROR: unexpected end of input #{type} on line #{location} with value '#{inspect(value)}' \n"
  end

  def to_string_response({:error, {location, :erl_parse, descriptor}}) do
    "ERROR : #{:erl_parse.format_error(descriptor)} in line #{location}\n"
  end

  def to_string_response({:error, {location, :erl_scan, descriptor}}) do
    case descriptor do
      {:unterminated, what, head} ->
        head_str = List.to_string(head)
        "ERROR: unterminated #{what} starting with '#{inspect(head_str)}' in line #{location}\n"
      {:illegal, type} ->
        "ERROR: illegal #{type} in line #{location}\n"
      {:base, base} ->
        "ERROR: illegal base '#{base}' in line #{location}\n"
      other ->
        "ERROR: #{:erl_scan.format_error(other)} in line #{location}\n"
    end
  end

  def to_string_response({:error, {location, :erl_lint, descriptor}}) do
    "#{:erl_lint.format_error(descriptor)} in line #{location}\n"
  end

  def to_string_response({:error, [{location, :erl_lint, descriptor} | tail]}) do
    "#{:erl_lint.format_error(descriptor)} in line #{location}\n" <> to_string_response({:error, tail})
  end

  def to_string_response({:error, msg}) when is_binary(msg) do
    "ERROR: #{msg}\n"
  end

  def to_string_response({:error, msg}) when is_list(msg) do

    cond do
      Enum.all?(msg, &is_integer/1) ->
        # It's a charlist
        "ERROR: #{List.to_string(msg)}\n"

      match?([{_, :erl_lint, _} | _], msg) ->
        # It's a list of erl_lint errors
        Enum.map(msg, fn {location, :erl_lint, descriptor} ->
          "#{:erl_lint.format_error(descriptor)} in line #{location}\n"
        end)
        |> Enum.join("")

      Enum.all?(msg, &is_list/1) ->
        # It's a list of charlists
        str =
          msg
          |> Enum.map(&List.to_string/1)
          |> Enum.join(" ")
        "ERROR: #{str}\n"

      true ->
        # Fallback for any other list
        "ERROR: #{inspect(msg)}\n"
    end
  end

  def to_string_response({:error, msg}) do
    "ERROR: #{inspect(msg)}\n"
  end
end
