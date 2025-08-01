defmodule ErlangSandbox.Server do
  import ErlangSandbox.HandleError, only: [to_string_response: 1]
  import ErlangSandbox.Erlang.Loader, only: [handle_erlang_code: 1]
  import ErlangSandbox.Tests, only: [run_tests: 2]

  def start(port \\ 4000) do
    {:ok, listen_socket} =
      :gen_tcp.listen(port, [:binary, packet: 0, active: false, reuseaddr: true])

    IO.puts("Listening on port #{port}")
    loop_acceptor(listen_socket)
  end

  defp loop_acceptor(listen_socket) do
    {:ok, client_socket} = :gen_tcp.accept(listen_socket)
    spawn(fn -> handle_client(client_socket) end)
    loop_acceptor(listen_socket)
  end

  defp handle_client(socket) do
    case :gen_tcp.recv(socket, 0) do
      {:ok, data} ->
          case Jason.decode(data) do
            {:ok, %{"op" => "compile", "code" => code}} ->
              case handle_erlang_code(code) do
                {:ok, response} ->
                  send_json(socket, %{status: "ok", result: to_string_response({:ok, response})})
                {:error, reason} ->
                  send_json(socket, %{status: "error", reason: to_string_response({:error, reason})})
              end

            {:ok, %{"op" => "test", "code" => code, "cases" => cases}} ->
              case run_tests(code, cases) do
                {:ok, response} ->
                  send_json(socket, %{status: "ok", result: to_string_response({:ok, response})})
                {:error, reason} ->
                  send_json(socket, %{status: "error", reason: to_string_response({:error, reason})})
              end

            {:error, reason} ->
             send_json(socket, %{status: "error", reason: to_string_response({:error, reason})})
          end

      {:error, reason} ->
        IO.puts(to_string_response({:error, reason}))
        :gen_tcp.close(socket)
    end
  end

  defp send_json(socket, data) do
    json = Jason.encode!(data)
    :gen_tcp.send(socket, json)
  end
end
