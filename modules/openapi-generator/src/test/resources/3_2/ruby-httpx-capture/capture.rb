# frozen_string_literal: true

# Wire-level verification harness for OpenAPI 3.2 support in the generated
# `ruby` (httpx) client. RubyClientCodegenTest copies this file into the
# generated client directory and runs it with `ruby capture.rb <generated lib dir>`.
#
# A raw TCP listener records the HTTP request line (and body, when present)
# for every generated call: standard methods keep the normal HTTPX dispatch,
# while query/ additionalOperations methods and `in: querystring` parameters
# must reach the wire verbatim (no up-casing, no re-encoding, no `//`
# collapsing inside the query component).
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
    cl = 0
    while (h = s.gets) && h != "\r\n"
      cl = h.split(':', 2)[1].strip.to_i if h.downcase.start_with?('content-length:')
    end
    body = cl.positive? ? (s.read(cl) || '') : ''
    lines << "#{line.strip} [CL=#{cl}]#{body.empty? ? '' : " BODY=#{body}"}"
    s.write "HTTP/1.1 200 OK\r\nContent-Length: 2\r\nConnection: close\r\n\r\n{}"
    s.close
  end
end

config = OpenapiClient::Configuration.new
config.host = "127.0.0.1:#{port}"
config.scheme = 'http'
config.debugging = false
api = OpenapiClient::DefaultApi.new(OpenapiClient::ApiClient.new(config))

# `in: querystring` callers pass the query component without the leading `?`;
# embedded `//` must reach the wire intact (path-only slash collapsing).
api.query_pets('a=1&u=http://h//p')
api.custom_pets
api.check_fetch_pets
api.purge_pets
api.hash_pets
api.list_pets
# QUERY carrying a request body, querystring param named `uri`
api.search_items('k=v', { 'a' => 1 })
# additionalOperations: REPORT with querystring, PROPPATCH with a body
api.report_items('r=1')
api.prop_patch('<x/>')

expected = [
  'QUERY /pets?a=1&u=http://h//p HTTP/1.1 [CL=0]',
  'customMethod /pets HTTP/1.1 [CL=0]',
  'CHECK&FETCH /pets HTTP/1.1 [CL=0]',
  'PURGE /pets HTTP/1.1 [CL=0]',
  'X#Y /pets HTTP/1.1 [CL=0]',
  'GET /pets HTTP/1.1 [CL=0]',
  'QUERY /items?k=v HTTP/1.1 [CL=7] BODY={"a":1}',
  'REPORT /report?r=1 HTTP/1.1 [CL=0]',
  'PROPPATCH /report HTTP/1.1 [CL=4] BODY=<x/>'
]
got = expected.size.times.map { lines.pop }
got.each_with_index do |line, i|
  warn("FAIL: got #{line.inspect}, want #{expected[i].inspect}") unless line == expected[i]
end
abort('CAPTURE-FAIL') unless got == expected
puts 'CAPTURE-PASS'
