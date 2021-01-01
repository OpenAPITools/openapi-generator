require "spec"
require "../src/*"

include Kemal

class CustomLogHandler < Kemal::BaseLogHandler
  def call(env)
    call_next env
  end

  def write(message)
  end
end

class TestContextStorageType
  property id
  @id = 1

  def to_s
    @id
  end
end

class AnotherContextStorageType
  property name
  @name = "kemal-context"
end

add_context_storage_type(TestContextStorageType)
add_context_storage_type(AnotherContextStorageType)

def create_request_and_return_io_and_context(handler, request)
  io = IO::Memory.new
  response = HTTP::Server::Response.new(io)
  context = HTTP::Server::Context.new(request, response)
  handler.call(context)
  response.close
  io.rewind
  {io, context}
end

def create_ws_request_and_return_io_and_context(handler, request)
  io = IO::Memory.new
  response = HTTP::Server::Response.new(io)
  context = HTTP::Server::Context.new(request, response)
  begin
    handler.call context
  rescue IO::Error
    # Raises because the IO::Memory is empty
  end
  {% if compare_versions(Crystal::VERSION, "0.35.0-0") >= 0 %}
    response.upgrade_handler.try &.call(io)
  {% end %}
  io.rewind
  {io, context}
end

def call_request_on_app(request)
  io = IO::Memory.new
  response = HTTP::Server::Response.new(io)
  context = HTTP::Server::Context.new(request, response)
  main_handler = build_main_handler
  main_handler.call context
  response.close
  io.rewind
  HTTP::Client::Response.from_io(io, decompress: false)
end

def build_main_handler
  Kemal.config.setup
  main_handler = Kemal.config.handlers.first
  current_handler = main_handler
  Kemal.config.handlers.each do |handler|
    current_handler.next = handler
    current_handler = handler
  end
  main_handler
end

Spec.before_each do
  config = Kemal.config
  config.env = "development"
  config.logging = false
end

Spec.after_each do
  Kemal.config.clear
  Kemal::RouteHandler::INSTANCE.routes = Radix::Tree(Route).new
  Kemal::RouteHandler::INSTANCE.cached_routes = Hash(String, Radix::Result(Route)).new
  Kemal::WebSocketHandler::INSTANCE.routes = Radix::Tree(WebSocket).new
end
