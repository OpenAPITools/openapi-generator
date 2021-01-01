require "../src/http_proxy"

def with_proxy_server(host = "127.0.0.1", port = 8080)
  wants_close = Channel(Nil).new

  server = HTTP::Proxy::Server.new(handlers: [
    HTTP::LogHandler.new,
    HTTP::Proxy::Server::BasicAuth.new("user", "passwd"),
  ]) do |context|
    context.request.headers.add("X-Forwarded-For", host)
  end

  spawn do
    server.bind_tcp(host, port)
    puts "start proxy server"
    server.listen
  end

  spawn do
    wants_close.receive
    puts "exit proxy server"
    server.close
  end

  Fiber.yield

  yield host, port, wants_close
end

with_proxy_server do |_host, _port, wants_close|
  puts "start proxy client"

  puts "HTTP Request:"
  uri = URI.parse("http://httpbin.org")
  proxy_client = HTTP::Proxy::Client.new("127.0.0.1", 8080, username: "user", password: "passwd")

  response = HTTP::Client.new(uri) do |client|
    client.set_proxy(proxy_client)
    client.get("http://httpbin.org/get")
  end

  puts response.status_code
  puts response.body

  puts "HTTPS Request:"
  uri = URI.parse("https://httpbin.org")
  proxy_client = HTTP::Proxy::Client.new("127.0.0.1", 8080, username: "user", password: "passwd")

  response = HTTP::Client.new(uri) do |client|
    client.set_proxy(proxy_client)
    client.get("/get")
  end

  puts response.status_code
  puts response.body

  puts "exit proxy client"
ensure
  wants_close.send(nil)
end
