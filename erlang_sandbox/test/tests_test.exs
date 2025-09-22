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
    erlang_source = "-module(test). -export([sum/2]). sum(A, B) -> A + B."

    cases = """
    defmodule Test do
      use ExUnit.Case

      test "sum" do
        assert :test.sum(1, 1) == 2
      end
    end
    """

    capture_log(fn ->
      {:ok, output, test_results} = ErlangSandbox.Tests.run_tests(erlang_source, cases)
      assert is_binary(output)
      assert is_map(test_results)
      assert test_results[:failures] == 0
    end)
  end
end
