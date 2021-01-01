module Kemal
  # Initializes the context with default values, such as
  # *Content-Type* or *X-Powered-By* headers.
  class InitHandler
    include HTTP::Handler

    INSTANCE = new

    def call(context : HTTP::Server::Context)
      context.response.headers.add "X-Powered-By", "Kemal" if Kemal.config.powered_by_header
      context.response.content_type = "text/html" unless context.response.headers.has_key?("Content-Type")
      call_next context
    end
  end
end
