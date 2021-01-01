require "../src/http_proxy"
require "option_parser"

host = "127.0.0.1"
port = 8080
username = "user"
password = "1234"

OptionParser.parse do |opts|
  opts.on("-h HOST", "--host HOST", "define host to run server") do |opt|
    host = opt
  end

  opts.on("-p PORT", "--port PORT", "define port to run server") do |opt|
    port = opt.to_i
  end

  opts.on("-u USER", "--user USER", "define user for authentication") do |opt|
    username = opt
  end

  opts.on("--pass PASSWORD", "define password for authentication") do |opt|
    password = opt
  end
end

server = HTTP::Proxy::Server.new(handlers: [
  HTTP::LogHandler.new,
  HTTP::Proxy::Server::BasicAuth.new(username, password),
]) do |context|
  context.request.headers.add("X-Forwarded-For", host)
  context.perform
end

address = server.bind_tcp(host, port)
puts "Listening on http://#{address}"
puts "Use #{username}:#{password} for authentication"
server.listen
