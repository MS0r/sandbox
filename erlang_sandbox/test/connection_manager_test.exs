defmodule ErlangSandbox.ConnectionManagerTest do
  use ExUnit.Case
  import ExUnit.CaptureLog

  setup do
    :meck.new(AMQP.Connection, [:non_strict])
    :meck.new(AMQP.Channel, [:non_strict])

    on_exit(fn ->
      :meck.unload()
    end)

    :ok
  end

  test "get_channel returns error when not connected" do
    :meck.expect(AMQP.Connection, :open, fn _url ->
      {:error, :econnrefused}
    end)

    capture_log(fn->
      ErlangSandbox.ConnectionManager.start_link(5672)
      assert {:error, :not_connected} = ErlangSandbox.ConnectionManager.get_channel()
    end)
  end

  test "get_channel opens a channel when connected" do
    conn = %{pid: self()}
    chan = %{pid: self()}

    :meck.expect(AMQP.Connection, :open, fn _url ->
      {:ok, conn}
    end)

    :meck.expect(AMQP.Channel, :open, fn _conn ->
      {:ok, chan}
    end)

    ErlangSandbox.ConnectionManager.start_link(5672)

    assert {:ok, ^chan} = ErlangSandbox.ConnectionManager.get_channel()
  end
end
