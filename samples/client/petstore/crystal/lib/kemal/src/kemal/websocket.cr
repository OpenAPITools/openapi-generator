module Kemal
  # Takes 2 parameters: *path* and a *handler* to specify
  # what action to be done if the route is matched.
  class WebSocket < HTTP::WebSocketHandler
    getter proc

    def initialize(@path : String, &@proc : HTTP::WebSocket, HTTP::Server::Context -> Void)
    end

    def call(context : HTTP::Server::Context)
      super
    end
  end
end
