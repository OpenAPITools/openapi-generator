# HTTP::Proxy

![Crystal CI](https://github.com/mamantoha/http_proxy/workflows/Crystal%20CI/badge.svg)
[![GitHub release](https://img.shields.io/github/release/mamantoha/http_proxy.svg)](https://github.com/mamantoha/http_proxy/releases)
[![License](https://img.shields.io/github/license/mamantoha/http_proxy.svg)](https://github.com/mamantoha/http_proxy/blob/master/LICENSE)

A HTTP Proxy server and client written in Crystal

## Installation

Add this to your application's `shard.yml`:

```yaml
dependencies:
  http_proxy:
    github: mamantoha/http_proxy
```

## Usage

### Server

```crystal
require "http_proxy"

host = "127.0.0.1"
port = 8080

server = HTTP::Proxy::Server.new

address = server.bind_tcp(host, port)
puts "Listening on http://#{address}"
server.listen
```

```crystal
require "http_proxy"
require "option_parser"

host = "192.168.0.1"
port = 3128

OptionParser.parse! do |opts|
  opts.on("-h HOST", "--host HOST", "define host to run server") do |opt|
    host = opt
  end

  opts.on("-p PORT", "--port PORT", "define port to run server") do |opt|
    port = opt.to_i
  end
end

server = HTTP::Proxy::Server.new(handlers: [
  HTTP::LogHandler.new,
]) do |context|
  context.perform
end

address = server.bind_tcp(host, port)
puts "Listening on http://#{address}"
server.listen
```

#### Basic Authentication

```crystal
server = HTTP::Proxy::Server.new(handlers: [
  HTTP::LogHandler.new,
  HTTP::Proxy::Server::BasicAuth.new("user", "passwd"),
]) do |context|
  context.request.headers.add("X-Forwarded-For", "127.0.0.1")
  context.perform
end
```

### Client

#### Make request with proxy

```crystal
require "http_proxy"

proxy_client = HTTP::Proxy::Client.new("127.0.0.1", 8080)

uri = URI.parse("http://httpbin.org")
client = HTTP::Client.new(uri)
client.set_proxy(proxy_client)
response = client.get("/get")
```

#### Client Authentication

```crystal
uri = URI.parse("https://httpbin.org")
proxy_client = HTTP::Proxy::Client.new("127.0.0.1", 8080, username: "user", password: "passwd")

response = HTTP::Client.new(uri) do |client|
  client.set_proxy(proxy_client)
  client.get("/get")
end

puts response.status_code
puts response.body
```

## Development

### Proxy server

* [x] Basic HTTP Proxy: GET, POST, PUT, DELETE support
* [x] Basic HTTP Proxy: OPTIONS support
* [x] HTTPS Proxy: CONNECT support
* [x] Make context.request & context.response writable
* [x] Basic Authentication
* [ ] MITM HTTPS Proxy

### Proxy client

* [x] Basic HTTP Proxy: GET, POST, PUT, DELETE support
* [x] Basic HTTP Proxy: OPTIONS support
* [x] HTTPS Proxy: CONNECT support
* [x] Basic Authentication

## Contributing

1. Fork it (<https://github.com/mamantoha/http_proxy/fork>)
2. Create your feature branch (git checkout -b my-new-feature)
3. Commit your changes (git commit -am 'Add some feature')
4. Push to the branch (git push origin my-new-feature)
5. Create a new Pull Request

## Contributors

* [bbtfr](https://github.com/bbtfr) Theo Li - creator, maintainer
* [mamantoha](https://github.com/mamantoha) Anton Maminov - maintainer
