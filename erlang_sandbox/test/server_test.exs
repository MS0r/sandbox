ExUnit.start()

defmodule ErlangSandbox.ServerTest do
  use ExUnit.Case
  import ExUnit.CaptureLog

  test "server responds to compile requests with ok or error" do
    capture_log(fn ->
      {:ok, port, listen_socket} = ErlangSandbox.Server.start_ephemeral()

      {:ok, sock} =
        :gen_tcp.connect(~c'127.0.0.1', port, [:binary,{:packet, 0}, active: false, packet: 0], 2000)

      # valid Erlang code -> compile path (may produce output)
      code = """
      -module(test_serv).
      -export([start/0]).
      start() -> io:format("hello_server~n").
      """

      :ok = :gen_tcp.send(sock, Jason.encode!(%{"op" => "compile", "code" => code}))
      {:ok, resp_bin} = :gen_tcp.recv(sock, 0, 2000)
      resp = Jason.decode!(resp_bin)
      assert resp["status"] in ["ok", "error"]

      :gen_tcp.close(sock)
      :gen_tcp.close(listen_socket)
    end)
  end

  test "server returns error for invalid erlang source" do
    capture_log(fn ->
      {:ok, port, listen_socket} = ErlangSandbox.Server.start_ephemeral()

      {:ok, sock} =
        :gen_tcp.connect(~c'127.0.0.1', port, [:binary,{:packet, 0}, active: false, packet: 0], 2000)

      invalid = "this is not erlang."
      :ok = :gen_tcp.send(sock, Jason.encode!(%{"op" => "compile", "code" => invalid}))
      {:ok, resp_bin} = :gen_tcp.recv(sock, 0, 2000)
      resp = Jason.decode!(resp_bin)
      assert resp["status"] == "error"
      assert is_binary(resp["reason"]) or is_map(resp["reason"])

      :gen_tcp.close(sock)
      :gen_tcp.close(listen_socket)
    end)
  end
end
