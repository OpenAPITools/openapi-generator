require "http"
require "lucky_flow"
require "../src/exception_page"
require "./support/*"

include LuckyFlow::Expectations

server = TestServer.new

LuckyFlow.configure do |settings|
  settings.base_uri = "http://#{server.addr}"
  settings.stop_retrying_after = 40.milliseconds
end

spawn do
  server.listen
end

at_exit do
  LuckyFlow.shutdown
  server.close
end

Habitat.raise_if_missing_settings!

require "spec"
