# config/prod.exs
import Config

# Use environment variable for port, default to 4000
port =
  case System.get_env("PORT") do
    nil -> 4000
    val -> String.to_integer(val)
  end

config :erlang_sandbox,
  port: port

# Logger: info level in production
config :logger, :console,
  level: :info,
  format: "[$level] $message\n"

# Optional: runtime-specific options
# For example, enabling SSL, metrics, etc.
