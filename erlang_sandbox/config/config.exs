import Config

config :logger, :default_formatter,
  format: "$time $metadata[$level] $message\n",
  metadata: [:file, :line,:correlation_id]

import_config("#{config_env()}.exs")
