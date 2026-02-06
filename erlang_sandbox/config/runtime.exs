import Config
import Dotenvy

if config_env() != :test do
  env_dir_prefix = System.get_env("RELEASE_ROOT") || Path.expand("./")

  source!([
    Path.absname(".env", env_dir_prefix),
    Path.absname("../.env",env_dir_prefix),
    Path.absname(".#{config_env()}.env", env_dir_prefix),
    Path.absname(".#{config_env()}.overrides.env", env_dir_prefix),
    System.get_env()
  ])


  config :erlang_sandbox,
    env: config_env(),
    port: env!("RABBITMQ_PORT", :number!, 5672),
    host: env!("RABBITMQ_HOST", :string!),
    user: env!("RABBITMQ_DEFAULT_USER", :string),
    password: env!("RABBITMQ_DEFAULT_PASS", :string)
end
