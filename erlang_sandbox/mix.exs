defmodule ErlangSandbox.MixProject do
  use Mix.Project

  def project do
    [
      app: :erlang_sandbox,
      version: "0.1.0",
      elixir: "~> 1.18",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      releases: [
        erlang_sandbox: [
          include_executables_for: [:unix],
          steps: [:assemble, :tar],
          applications: [runtime_tools: :permanent]
        ]
      ],
      test_coverage: [tool: ExCoveralls]
    ]
  end

  def cli do
    [preferred_envs: [coveralls: :test, "coveralls.detail": :test, "coveralls.post": :test, "coveralls.html": :test]]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      applications: [:logger, :ex_unit, :amqp],
      mod: {ErlangSandbox.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps() do
    [
      {:jason, "~> 1.4"},
      {:excoveralls, "~> 0.16", only: :test},
      {:amqp, "~> 4.1"}
      # {:dep_from_hexpm, "~> 0.3.0"},
      # {:dep_from_git, git: "https://github.com/elixir-lang/my_dep.git", tag: "0.1.0"}
    ]
  end
end
