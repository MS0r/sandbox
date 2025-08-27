defmodule ErlangSandboxTest do
  use ExUnit.Case
  import ExUnit.CaptureLog

  test "compile_and_load_erlang_module compiles and loads valid erlang module" do
    erlang_source = """
    -module(test_mod).
    -export([start/0]).
    start() -> io:format("hello_erlang~n").
    """

    assert {:ok, module} = ErlangSandbox.Erlang.Loader.compile_and_load_erlang_module(erlang_source)
    assert is_atom(module)

    # run_module/1 returns a tuple where the second element is the captured output
    result = ErlangSandbox.Erlang.Loader.run_module({:run, module})
    assert is_tuple(result)
    assert elem(result, 1) =~ "hello_erlang"
  end

  test "compile_and_load_erlang_module returns error for invalid erlang source" do
    invalid_source = "this is not erlang."
    assert {:error, _reason} = ErlangSandbox.Erlang.Loader.compile_and_load_erlang_module(invalid_source)
  end

  test "handle_erlang_code returns error for runtime error in erlang module" do
    erlang_source = """
    -module(err_mod).
    -export([start/0]).
    start() -> erlang:error(badarg).
    """

    # capture the error log so test output stays clean
    capture_log(fn ->
      assert {:error, msg} = ErlangSandbox.Erlang.Loader.handle_erlang_code(erlang_source)
      assert is_binary(msg)
      final = ErlangSandbox.HandleError.to_string_response({:error, msg})
      assert final =~ "bad argument"
    end)
  end

  test "handle_erlang_code returns timeout error for long running erlang code" do
    erlang_source = """
    -module(slow_mod).
    -export([start/0]).
    start() -> timer:sleep(6000), io:format("done~n").
    """

    capture_log(fn ->
      assert {:error, msg} = ErlangSandbox.Erlang.Loader.handle_erlang_code(erlang_source)
      assert msg =~ "TimeoutError"
    end)
  end

  test "run_tests returns error when no test module compiled from cases" do
    valid_erlang = """
    -module(ok_mod).
    -export([hello/0]).
    hello() -> ok.
    """

    # provide empty cases so Code.compile_string(cases) returns []
    assert {:error, "No module compiled from test cases"} =
             ErlangSandbox.Tests.run_tests(valid_erlang, "")
  end

  test "erl_scan unterminated string is formatted" do
    res =
      ErlangSandbox.HandleError.to_string_response(
        {:error, {1, :erl_scan, {:unterminated, :string, ~c'abc'}}}
      )

    assert res =~ "unterminated string"
    assert res =~ "abc"
    assert res =~ "in line 1"
  end

  test "erl_scan illegal type is formatted" do
    res =
      ErlangSandbox.HandleError.to_string_response(
        {:error, {2, :erl_scan, {:illegal, :dot}}}
      )

    assert res =~ "illegal"
    assert res =~ "dot"
    assert res =~ "in line 2"
  end

  test "erl_scan base error is formatted" do
    res =
      ErlangSandbox.HandleError.to_string_response(
        {:error, {3, :erl_scan, {:base, ~c'16'}}}
      )

    assert res =~ "illegal base"
    assert res =~ "in line 3"
  end

  test "erl_scan indentation error is formatted" do
    res = ErlangSandbox.HandleError.to_string_response({:error, {4, :erl_scan, :indentation}})
    assert res =~ "bad indentation in triple-quoted string"
    assert res =~ "in line 4"
  end

  test "erl_scan white_space error is formatted" do
    res = ErlangSandbox.HandleError.to_string_response({:error, {5, :erl_scan, :white_space}})
    assert res =~ "not white space after start of triple-quoted string"
    assert res =~ "in line 5"
  end

  test "erl_scan string_concat error is formatted" do
    res = ErlangSandbox.HandleError.to_string_response({:error, {6, :erl_scan, :string_concat}})
    assert res =~ "adjacent string literals without intervening white space"
    assert res =~ "in line 6"
  end

  test "erl_scan unterminated character (two-arity) is formatted" do
    res = ErlangSandbox.HandleError.to_string_response({:error, {7, :erl_scan, {:unterminated, ?a}}})
    assert res =~ "unterminated character" or res =~ "unterminated"
    assert res =~ "in line 7"
  end
end
