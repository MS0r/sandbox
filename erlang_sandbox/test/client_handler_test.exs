defmodule ErlangSandbox.RequestHandlerTest do
  use ExUnit.Case
  import ExUnit.CaptureLog

  test "handle_request compiles erlang code and returns output" do
    code = """
    -module(test_client).
    -export([start/0]).
    start() -> io:format("client_ok~n").
    """

    capture_log(fn ->
      assert {:ok, output} =
               ErlangSandbox.RequestHandler.handle_request({:ok, %{"op" => "compile", "code" => code}})

      assert output =~ "client_ok"
    end)
  end

  test "handle_request compiles erlang code and returns error unexpected end of input" do
    code = """
    -module(test_client).
    -export([start/0]).
    start() -> io:format("client_ok~n")
    """

    capture_log(fn ->
      assert {:error, output} =
               ErlangSandbox.RequestHandler.handle_request({:ok, %{"op" => "compile", "code" => code}})

      assert is_tuple(output)
      ls = Tuple.to_list(output)
      assert Enum.member?(ls, :unexpected_end_of_input)
    end)
  end

  test "handle_request compiles erlang code and returns error" do
    code = """
    -module(module).
    -export([start/1]).
    start() -> io:format("client_ok~n").
    """

    capture_log(fn ->
      assert {:error, output} =
               ErlangSandbox.RequestHandler.handle_request({:ok, %{"op" => "compile", "code" => code}})

      assert is_list(output)
      ls = Tuple.to_list(Enum.at(output,0))
      assert Enum.member?(ls, :erl_lint)
    end)
  end

  test "handle_request runs test cases and returns results" do
    code = """
    -module(test_client).
    -export([sum/2]).
    sum(A, B) -> A + B.
    """

    cases = """
    defmodule RequestHandlerTestCases1 do
      use ExUnit.Case

      test "sum" do
        assert :test_client.sum(2, 3) == 5
      end
    end
    """

    capture_log(fn ->
      assert {:ok, _output, results} =
               ErlangSandbox.RequestHandler.handle_request(
                 {:ok, %{"op" => "test", "code" => code, "cases" => cases}}
               )

      assert results[:failures] == 0
      assert results[:total] == 1
    end)
  end

  test "handle_request runs test cases and returns failure" do
    code = """
    -module(test_client).
    -export([sum/2]).
    sum(A, B) -> A - B.
    """

    cases = """
    defmodule RequestHandlerTestCases2 do
      use ExUnit.Case

      test "sum" do
        assert :test_client.sum(2, 3) == 5
      end
    end
    """

    capture_log(fn ->
      assert {:ok, _output, results} =
               ErlangSandbox.RequestHandler.handle_request(
                 {:ok, %{"op" => "test", "code" => code, "cases" => cases}}
               )

      assert results[:failures] == 1
      assert results[:total] == 1
    end)
  end


  test "handle_request returns error for malformed payload" do
    assert {:error, "Payload malformed"} =
             ErlangSandbox.RequestHandler.handle_request({:ok, %{"foo" => "bar"}})
  end
end
