require "./server/handler"
require "./server/basic_auth"

# A concurrent Proxy server implementation.
#
# ```
# require "http_proxy"
#
# server = HTTP::Proxy::Server.new
# address = server.bind_tcp("127.0.0.1", 8080)
# puts "Listening on http://#{address}"
# server.listen
# ```
#
class HTTP::Proxy::Server
  def initialize
    handler = build_middleware
    @processor = RequestProcessor.new(handler)
  end

  def initialize(&handler : Context ->)
    @processor = RequestProcessor.new(handler)
  end

  def initialize(handlers : Array(HTTP::Handler), &handler : Context ->)
    handler = build_middleware(handlers, handler)
    @processor = RequestProcessor.new(handler)
  end

  def initialize(handlers : Array(HTTP::Handler))
    handler = build_middleware(handlers)
    @processor = RequestProcessor.new(handler)
  end

  def initialize(handler : HTTP::Handler | HTTP::Handler::HandlerProc)
    @processor = RequestProcessor.new(handler)
  end

  private def build_middleware(handler : (Context ->)? = nil)
    proxy_handler = Handler.new
    proxy_handler.next = handler if handler
    proxy_handler
  end

  private def build_middleware(handlers, last_handler : (Context ->)? = nil)
    proxy_handler = build_middleware(last_handler)
    return proxy_handler if handlers.empty?

    0.upto(handlers.size - 2) { |i| handlers[i].next = handlers[i + 1] }
    handlers.last.next = proxy_handler if proxy_handler
    handlers.first
  end
end
