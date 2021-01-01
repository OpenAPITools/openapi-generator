require "http"
require "uri"
require "base64"
require "http-client-digest_auth"
require "http_proxy"

# This module's static methods are the entry point for using the Crest client.
#
# Suported HTTP methods: `get`, `put`, `post`, `patch` `delete`, `options`, `head`
#
# Examples:
#
# ```crystal
# Crest.get(
#   "http://httpbin.org/get",
#   headers: {"Content-Type" => "image/jpg"},
#   params: {"lang" => "en"}
# )
#
# Crest.post(
#   "http://httpbin.org/post",
#   headers: {"Access-Token" => ["secret1", "secret2"]},
#   form: {:fizz => "buz"},
#   logging: true,
# )
#
# Crest.get("http://httpbin.org/stream/5") do |response|
#   while line = response.body_io.gets
#     puts line
#   end
# end
# ```
module Crest
  VERSION = {{ `shards version #{__DIR__}`.chomp.stringify }}

  alias TextValue = String | Symbol | Int32 | Bool | Nil

  alias Params = Hash(Symbol | String, Int32 | String) |
                 Hash(String, String | Int32) |
                 Hash(Symbol, String | Int32) |
                 Hash(String, String) |
                 Hash(String, Int32) |
                 Hash(Symbol, String) |
                 Hash(Symbol, Int32)

  HTTP_METHODS = %w{get delete post put patch options head}

  {% for method in Crest::HTTP_METHODS %}
    # Execute a {{method.id.upcase}} request and and yields the `Crest::Response` to the block.
    #
    # ```crystal
    # Crest.{{method.id}}("http://httpbin.org/{{method.id}}") do |response|
    #   while line = response.body_io.gets
    #     puts line
    #   end
    # end
    # ```
    def self.{{method.id}}(url : String, **args, &block : Crest::Response ->) : Nil
      request = Request.new(:{{method.id}}, url, **args)
      request.execute(&block)
    end

    # Execute a {{method.id.upcase}} request and returns a `Crest::Response`.
    #
    # ```crystal
    # Crest.{{method.id}}("http://httpbin.org/{{method.id}}")
    # ```
    def self.{{method.id}}(url : String, **args) : Crest::Response
      request = Request.new(:{{method.id}}, url, **args)
      request.execute
    end
  {% end %}
end

require "./crest/**"
