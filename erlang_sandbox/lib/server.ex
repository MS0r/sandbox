defmodule ErlangSandbox.Server do
  use GenServer
  import ErlangSandbox.HandleError, only: [to_string_response: 1]
  import ErlangSandbox.ClientHandler, only: [handle_request: 1]

  def start_link(port \\ 4000, accept \\ true) do
    GenServer.start_link(__MODULE__, {port, accept}, name: __MODULE__)
  end

  def start_ephemeral do
    {:ok, pid} = GenServer.start_link(__MODULE__, {0, false})
    port = GenServer.call(pid, :get_port)
    {:ok, port, pid}
  end

  def init({port, accept}) do
    {:ok, listen_socket} =
      :gen_tcp.listen(port, [:binary, packet: 0, active: false, reuseaddr: true])

    {:ok, {_ip, actual_port}} = :inet.sockname(listen_socket)
    IO.puts("Listening on port #{actual_port}")

    # store state including socket
    state = %{listen_socket: listen_socket, port: actual_port}

    # start accepting connections in this process
    if accept do
      send(self(), :accept)
    end
    {:ok, state}
  end

  def handle_call(:get_port, _from, state) do
    {:reply, state.port, state}
  end

  def handle_info(:accept, %{listen_socket: listen_socket} = state) do
    case :gen_tcp.accept(listen_socket) do
      {:ok, client_socket} ->
        # handle client in separate process so we can continue accepting
        Task.start(fn -> handle_client(client_socket) end)
        # continue accepting
        send(self(), :accept)
        {:noreply, state}

      {:error, reason} ->
        IO.puts("Failed to accept connection: #{to_string(reason)}")
        {:stop, reason, state}
    end
  end

  defp handle_client(socket) do
    case :gen_tcp.recv(socket, 0) do
      {:ok, data} ->
        case handle_request(Jason.decode(data)) do
          {:ok, response} ->
            send_json(socket, %{status: "ok", result: to_string_response({:ok, response})})

          {:error, reason} ->
            send_json(socket, %{status: "error", reason: to_string_response({:error, reason})})

          {:ok, output, test_results} ->
            send_json(socket, %{
              status: "ok",
              result: to_string_response({:ok, output}),
              test_results: test_results
            })
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
