require "http"
require "socket"
require "base64"

require "./http/proxy/server"
require "./http/proxy/client"

module HTTP
  module Proxy
    VERSION = {{ `shards version #{__DIR__}`.chomp.stringify }}
  end
end
