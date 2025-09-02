# config/dev.exs
import Config

# Override default port for dev
config :erlang_sandbox,
  port: 4000

# Logger configuration
config :logger, :console,
  level: :debug,
  format: "[$level] $message\n"

# Optional: enable code reloading or other dev-specific features
