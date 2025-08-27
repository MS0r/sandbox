defmodule ErlangSandbox.TestsTest do
  use ExUnit.Case
  import ExUnit.CaptureLog

  test "returns error when no test module compiled from cases" do
    valid_erlang = """
    -module(ok_mod).
    -export([hello/0]).
    hello() -> ok.
    """

    assert {:error, "No module compiled from test cases"} =
             ErlangSandbox.Tests.run_tests(valid_erlang, "")
  end

  test "returns error for invalid erlang source" do
    invalid_source = "this is not erlang."

    cases = """
    defmodule TestCasesForInvalidSource do
      def run, do: :ok
    end
    """

    assert {:error, _reason} = ErlangSandbox.Tests.run_tests(invalid_source, cases)
  end

  test "runs test cases that call compiled erlang module" do
    erlang_source = """
    -module(test_run_mod).
    -export([start/0]).
    start() -> io:format("hello_from_erlang~n"), ok.
    """

    cases = """
    defmodule TestCasesRunSuccess do
      def run do
        :test_run_mod.start()
        :ok
      end
    end
    """

    capture_log(fn ->
      assert {:ok, output, test_results} = ErlangSandbox.Tests.run_tests(erlang_source, cases)
      assert is_binary(output)
      assert output =~ "Running ExUnit with seed"
      assert is_map(test_results) or is_list(test_results)
    end)
  end
end
