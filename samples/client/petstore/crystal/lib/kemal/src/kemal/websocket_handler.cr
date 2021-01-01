module Kemal
  class WebSocketHandler
    include HTTP::Handler

    INSTANCE = new
    property routes

    def initialize
      @routes = Radix::Tree(WebSocket).new
    end

    def call(context : HTTP::Server::Context)
      return call_next(context) unless context.ws_route_found? && websocket_upgrade_request?(context)
      context.websocket.call(context)
    end

    def lookup_ws_route(path : String)
      @routes.find "/ws" + path
    end

    def add_route(path : String, &handler : HTTP::WebSocket, HTTP::Server::Context -> Void)
      add_to_radix_tree path, WebSocket.new(path, &handler)
    end

    private def add_to_radix_tree(path, websocket)
      node = radix_path "ws", path
      @routes.add node, websocket
    end

    private def radix_path(method, path)
      '/' + method.downcase + path
    end

    private def websocket_upgrade_request?(context)
      return unless upgrade = context.request.headers["Upgrade"]?
      return unless upgrade.compare("websocket", case_insensitive: true) == 0

      context.request.headers.includes_word?("Connection", "Upgrade")
    end
  end
end
