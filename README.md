Before starting the server, you should add the following args to your .env:

```bash
RABBITMQ_HOST="hostname"
RABBITMQ_DEFAULT_USER="some-user"
RABBITMQ_DEFAULT_PASS="some-password"
```
also if you set some different port from the RabbitMQ's default use this:

```bash
RABBITMQ_PORT=1234
```

and to compile and initialize the server you should use the following:

```bash
# Depends on your environment
MIX_ENV=(dev,test,prod) iex -S mix
```