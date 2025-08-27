ExUnit.start()

# reduce noisy logs during tests
require Logger
Logger.configure(level: :warning)
