defmodule ErlangSandboxTest do
  use ExUnit.Case
  doctest ErlangSandbox

  test "greets the world" do
    assert ErlangSandbox.hello() == :world
  end
end
