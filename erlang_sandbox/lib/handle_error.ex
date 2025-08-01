defmodule ErlangSandbox.HandleError do
  def to_string_response({:ok, msg}), do: "#{msg}\n"

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

  def to_string_response({:error, msg}) do
  # If msg is a list of charlists, convert and join
  str =
    msg
    |> Enum.map(fn
      l when is_list(l) -> List.to_string(l)
      other -> inspect(other)
    end)
    |> Enum.join(" ")
  "ERROR: #{str}\n"
  end
end
