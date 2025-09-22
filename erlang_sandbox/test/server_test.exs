ExUnit.start()

defmodule ErlangSandbox.ServerTest do
  use ExUnit.Case
  import ExUnit.CaptureLog

  setup do
    {:ok, port, pid} = ErlangSandbox.Server.start_ephemeral()
    on_exit(fn -> Process.exit(pid, :kill) end)
    {:ok, port: port, pid: pid}
  end

  test "server responds to compile requests with ok", %{port: port, pid: pid} do
    capture_log(fn ->
      send(pid, :accept)

      {:ok, sock} =
        :gen_tcp.connect(
          ~c'127.0.0.1',
          port,
          [:binary, {:packet, 0}, active: false, packet: 0],
          2000
        )

      code = """
      -module(test_serv).
      -export([start/0]).
      start() -> io:format("hello_server~n").
      """

      :ok = :gen_tcp.send(sock, Jason.encode!(%{"op" => "compile", "code" => code}))
      {:ok, resp_bin} = :gen_tcp.recv(sock, 0, 2000)
      resp = Jason.decode!(resp_bin)
      assert resp["status"] == "ok"
      assert resp["result"] == "hello_server\n"
      :gen_tcp.close(sock)
    end)
  end

  test "server returns error for invalid erlang source", %{port: port, pid: pid} do
    capture_log(fn ->
      send(pid, :accept)

      {:ok, sock} =
        :gen_tcp.connect(
          ~c'127.0.0.1',
          port,
          [:binary, {:packet, 0}, active: false, packet: 0],
          2000
        )

      invalid = "this is not erlang."
      :ok = :gen_tcp.send(sock, Jason.encode!(%{"op" => "compile", "code" => invalid}))
      {:ok, resp_bin} = :gen_tcp.recv(sock, 0, 2000)
      resp = Jason.decode!(resp_bin)
      assert resp["status"] == "error"
      assert is_binary(resp["reason"]) or is_map(resp["reason"])

      :gen_tcp.close(sock)
    end)
  end

  test "server responds to test requests with ok", %{port: port, pid: pid} do
    capture_log(fn ->
      send(pid, :accept)

      {:ok, sock} =
        :gen_tcp.connect(
          ~c'127.0.0.1',
          port,
          [:binary, {:packet, 0}, active: false, packet: 0],
          2000
        )

      code = """
      -module(test_serv).
      -export([sum/2]).
      sum(A, B) -> A + B.
      """

      cases = """
      defmodule TestModTest do
        use ExUnit.Case

        test "sum" do
          assert :test_serv.sum(1, 2) == 3
        end
      end
      """

      :ok =
        :gen_tcp.send(sock, Jason.encode!(%{"op" => "test", "code" => code, "cases" => cases}))

      {:ok, resp_bin} = :gen_tcp.recv(sock, 0, 2000)
      resp = Jason.decode!(resp_bin)
      assert resp["status"] == "ok"
      assert resp["test_results"]["failures"] == 0
      assert resp["test_results"]["total"] == 1
      :gen_tcp.close(sock)
    end)
  end

  test "server responds to test requests with error on cases", %{port: port, pid: pid} do
    capture_log(fn ->
      send(pid, :accept)

      {:ok, sock} =
        :gen_tcp.connect(
          ~c'127.0.0.1',
          port,
          [:binary, {:packet, 0}, active: false, packet: 0],
          2000
        )

      code = """
      -module(test_serv).
      -export([sum/2]).
      sum(A, B) -> A + B.
      """

      cases = "invalid"

      :ok =
        :gen_tcp.send(sock, Jason.encode!(%{"op" => "test", "code" => code, "cases" => cases}))

      {:ok, resp_bin} = :gen_tcp.recv(sock, 0, 5000)
      resp = Jason.decode!(resp_bin)
      assert resp["status"] == "error"
      assert resp["reason"] =~ "compile"
      :gen_tcp.close(sock)
    end)
  end

  test "server responds to test requests with error on code", %{port: port, pid: pid} do
    capture_log(fn ->
      send(pid, :accept)

      {:ok, sock} =
        :gen_tcp.connect(
          ~c'127.0.0.1',
          port,
          [:binary, {:packet, 0}, active: false, packet: 0],
          2000
        )

      code = "aa"

      cases = "invali"

      :ok =
        :gen_tcp.send(sock, Jason.encode!(%{"op" => "test", "code" => code, "cases" => cases}))

      {:ok, resp_bin} = :gen_tcp.recv(sock, 0, 2000)
      resp = Jason.decode!(resp_bin)
       assert resp["status"] == "error"
      :gen_tcp.close(sock)
    end)
  end
end
