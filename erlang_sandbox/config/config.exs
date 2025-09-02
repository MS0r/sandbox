# config/config.exs
import Config

config :erlang_sandbox,
  # default TCP port (can be overridden per environment)
  port: 4000

# JSON library
config :jason, :decode, keys: :atoms
