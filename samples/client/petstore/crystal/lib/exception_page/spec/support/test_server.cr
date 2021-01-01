class TestServer
  getter addr : Socket::IPAddress

  delegate :listen, :close,
    to: @server

  def initialize(port : Int32? = nil)
    @server = HTTP::Server.new do |context|
      if context.request.resource == "/favicon.ico"
        context.response.print ""
      else
        begin
          raise CustomException.new("Something went very wrong")
        rescue e : CustomException
          context.response.content_type = "text/html"
          context.response.print MyApp::ExceptionPage.for_runtime_exception(context, e).to_s
        end
      end
    end
    if port
      @addr = @server.bind_tcp(port: port)
    else
      @addr = @server.bind_unused_port
    end
  end
end

class CustomException < Exception
end
