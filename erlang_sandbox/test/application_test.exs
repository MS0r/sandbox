defmodule ErlangSandbox.ApplicationTest do
  use ExUnit.Case
  import ExUnit.CaptureLog

  test "start/2 returns current process in test env" do
    previous_env = Application.get_env(:erlang_sandbox, :env)
    Application.put_env(:erlang_sandbox, :env, :test)

    on_exit(fn ->
      Application.put_env(:erlang_sandbox, :env, previous_env)
    end)

    capture_log(fn ->
      assert {:ok, pid} = ErlangSandbox.Application.start(:normal, [])
      assert pid == self()
    end)
  end
end
