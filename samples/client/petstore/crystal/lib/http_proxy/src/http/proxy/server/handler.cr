require "./context"

class HTTP::Proxy::Server::Handler
  include HTTP::Handler

  property next : HTTP::Handler | Proc | Nil

  alias Proc = Context ->

  def call(context)
    request = context.request
    response = context.response
    context = Context.new(request, response)

    context.perform
  end
end
