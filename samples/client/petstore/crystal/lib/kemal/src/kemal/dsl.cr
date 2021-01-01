# Kemal DSL is defined here and it's baked into global scope.
#
# The DSL currently consists of:
#
# - get post put patch delete options
# - WebSocket(ws)
# - before_*
# - error
HTTP_METHODS   = %w(get post put patch delete options)
FILTER_METHODS = %w(get post put patch delete options all)

{% for method in HTTP_METHODS %}
  def {{method.id}}(path : String, &block : HTTP::Server::Context -> _)
    raise Kemal::Exceptions::InvalidPathStartException.new({{method}}, path) unless Kemal::Utils.path_starts_with_slash?(path)
    Kemal::RouteHandler::INSTANCE.add_route({{method}}.upcase, path, &block)
  end
{% end %}

def ws(path : String, &block : HTTP::WebSocket, HTTP::Server::Context -> Void)
  raise Kemal::Exceptions::InvalidPathStartException.new("ws", path) unless Kemal::Utils.path_starts_with_slash?(path)
  Kemal::WebSocketHandler::INSTANCE.add_route path, &block
end

def error(status_code : Int32, &block : HTTP::Server::Context, Exception -> _)
  Kemal.config.add_error_handler status_code, &block
end

# All the helper methods available are:
#  - before_all, before_get, before_post, before_put, before_patch, before_delete, before_options
#  - after_all, after_get, after_post, after_put, after_patch, after_delete, after_options
{% for type in ["before", "after"] %}
  {% for method in FILTER_METHODS %}
    def {{type.id}}_{{method.id}}(path : String = "*", &block : HTTP::Server::Context -> _)
     Kemal::FilterHandler::INSTANCE.{{type.id}}({{method}}.upcase, path, &block)
    end
  {% end %}
{% end %}
