# config/test.exs
import Config

# Use an ephemeral port for testing
config :erlang_sandbox,
  port: 0

# Reduce log output in tests
config :logger, :console,
  level: :warn,
  format: "[$level] $message\n"
