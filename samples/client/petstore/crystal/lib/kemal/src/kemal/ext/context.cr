# `HTTP::Server::Context` is the class which holds `HTTP::Request` and
# `HTTP::Server::Response` alongside with information such as request params,
# request/response content_type, session data and alike.
#
# Instances of this class are passed to an `HTTP::Server` handler.
class HTTP::Server
  class Context
    # :nodoc:
    STORE_MAPPINGS = [Nil, String, Int32, Int64, Float64, Bool]

    macro finished
      alias StoreTypes = Union({{ *STORE_MAPPINGS }})
      @store = {} of String => StoreTypes
    end

    def params
      @params ||= Kemal::ParamParser.new(@request, route_lookup.params)
    end

    def redirect(url : String, status_code : Int32 = 302, *, body : String? = nil)
      @response.headers.add "Location", url
      @response.status_code = status_code
      @response.print(body) if body
    end

    def route
      route_lookup.payload
    end

    def websocket
      ws_route_lookup.payload
    end

    def route_lookup
      Kemal::RouteHandler::INSTANCE.lookup_route(@request.method.as(String), @request.path)
    end

    def route_found?
      route_lookup.found?
    end

    def ws_route_lookup
      Kemal::WebSocketHandler::INSTANCE.lookup_ws_route(@request.path)
    end

    def ws_route_found?
      ws_route_lookup.found?
    end

    def get(name : String)
      @store[name]
    end

    def set(name : String, value : StoreTypes)
      @store[name] = value
    end

    def get?(name : String)
      @store[name]?
    end
  end
end
