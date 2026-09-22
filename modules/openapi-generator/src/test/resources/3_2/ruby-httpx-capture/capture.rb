# frozen_string_literal: true

# Wire-level verification harness for OpenAPI 3.2 support in the generated
# `ruby` (httpx) client. RubyClientCodegenTest copies this file into the
# generated client directory and runs it with `ruby capture.rb <generated lib dir>`.
#
# A raw TCP listener records the HTTP request line for every generated call:
# standard methods keep the normal HTTPX dispatch, while query/
# additionalOperations methods and `in: querystring` parameters must reach the
# wire verbatim (no up-casing, no re-encoding).
$LOAD_PATH.unshift ARGV[0] || raise('usage: capture.rb <generated lib dir>')

require 'socket'
require 'openapi_client'

server = TCPServer.new('127.0.0.1', 0)
port = server.addr[1]
lines = Queue.new
Thread.new do
  loop do
    s = server.accept
    line = s.gets
    lines << line
    # consume request headers (no body in this fixture)
    while (h = s.gets) && h != "\r\n"; end
    s.write "HTTP/1.1 200 OK\r\nContent-Length: 2\r\nConnection: close\r\n\r\n{}"
    s.close
  end
end

config = OpenapiClient::Configuration.new
config.host = "127.0.0.1:#{port}"
config.scheme = 'http'
config.debugging = false
api = OpenapiClient::DefaultApi.new(OpenapiClient::ApiClient.new(config))

# `in: querystring` callers pass the query component without the leading `?`
api.query_pets('a=1&b=%20x')
api.custom_pets
api.check_fetch_pets
api.purge_pets
api.hash_pets
api.list_pets

expected = [
  'QUERY /pets?a=1&b=%20x HTTP/1.1',
  'customMethod /pets HTTP/1.1',
  'CHECK&FETCH /pets HTTP/1.1',
  'PURGE /pets HTTP/1.1',
  'X#Y /pets HTTP/1.1',
  'GET /pets HTTP/1.1'
]
got = 6.times.map { lines.pop.strip }
got.each_with_index do |line, i|
  warn("FAIL: got #{line.inspect}, want #{expected[i].inspect}") unless line == expected[i]
end
abort('CAPTURE-FAIL') unless got == expected
puts 'CAPTURE-PASS'
