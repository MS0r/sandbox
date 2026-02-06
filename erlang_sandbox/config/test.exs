import Config

config :logger, level: :error

config :erlang_sandbox,
  env: config_env(),
  port: 0,
  host: "localhost",
  user: "user",
  password: "password"
