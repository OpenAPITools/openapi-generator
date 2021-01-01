class HTTP::Proxy::Server < HTTP::Server
  class Context < HTTP::Server::Context
    def perform
      case @request.method
      when "OPTIONS"
        @response.headers["Allow"] = "OPTIONS,GET,HEAD,POST,PUT,DELETE,CONNECT"
      when "CONNECT"
        handle_tunneling
      else
        handle_http
      end
    end

    private def handle_tunneling
      host, port = @request.resource.split(":", 2)
      upstream = TCPSocket.new(host, port)

      @response.upgrade do |downstream|
        channel = Channel(Nil).new(2)

        downstream = downstream.as(TCPSocket)
        downstream.sync = true

        spawn do
          transfer(upstream, downstream, channel)
          transfer(downstream, upstream, channel)
        end

        2.times { channel.receive }
      end
    end

    private def transfer(destination, source, channel)
      spawn do
        IO.copy(destination, source)
      rescue ex
        Log.error(exception: ex) { "Unhandled exception on HTTP::Proxy::Server::Context" }
      ensure
        channel.send(nil)
      end
    end

    private def handle_http
      uri = URI.parse(@request.resource)
      client = HTTP::Client.new(uri)

      @request.headers.delete("Accept-Encoding")

      response = client.exec(@request)

      response.headers.delete("Transfer-Encoding")
      response.headers.delete("Content-Encoding")

      @response.headers.merge!(response.headers)
      @response.status_code = response.status_code
      @response.puts(response.body)
    end
  end
end
