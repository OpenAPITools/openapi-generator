require "spec"
require "json"
require "http_proxy"
require "../src/crest"
require "./support/server"

TEST_SERVER_HOST = "0.0.0.0"
TEST_SERVER_PORT = 4567
TEST_SERVER_URL  = "http://#{TEST_SERVER_HOST}:#{TEST_SERVER_PORT}"

kemal_config = Kemal.config
kemal_config.env = "development"
kemal_config.logging = false

spawn do
  Kemal.run(port: TEST_SERVER_PORT)
end

until Kemal.config.running
  sleep 1.millisecond
end

def with_proxy_server(host = "127.0.0.1", port = 8080)
  wants_close = Channel(Nil).new
  server = HTTP::Proxy::Server.new

  spawn do
    server.bind_tcp(host, port)
    server.listen
  end

  spawn do
    wants_close.receive
    server.close
  end

  Fiber.yield

  yield host, port, wants_close
end
