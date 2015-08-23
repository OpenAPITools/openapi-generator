require 'logger'

module Petstore
  module Swagger
    class Configuration
      attr_accessor :scheme, :host, :base_path, :user_agent, :format, :auth_token, :inject_format, :force_ending_format

      # Defines the username used with HTTP basic authentication.
      #
      # @return [String]
      attr_accessor :username

      # Defines the password used with HTTP basic authentication.
      #
      # @return [String]
      attr_accessor :password

      # Defines API keys used with API Key authentications.
      #
      # @return [Hash] key: parameter name, value: parameter value (API key)
      #
      # @example parameter name is "api_key", API key is "xxx" (e.g. "api_key=xxx" in query string)
      #   config.api_key['api_key'] = 'xxx'
      attr_accessor :api_key

      # Defines API key prefixes used with API Key authentications.
      #
      # @return [Hash] key: parameter name, value: API key prefix
      #
      # @example parameter name is "Authorization", API key prefix is "Token" (e.g. "Authorization: Token xxx" in headers)
      #   config.api_key_prefix['api_key'] = 'Token'
      attr_accessor :api_key_prefix

      # Set this to false to skip verifying SSL certificate when calling API from https server.
      # Default to true.
      #
      # @note Do NOT set it to false in production code, otherwise you would face multiple types of cryptographic attacks.
      #
      # @return [true, false]
      attr_accessor :verify_ssl

      # Set this to customize the certificate file to verify the peer.
      #
      # @return [String] the path to the certificate file
      #
      # @see The `cainfo` option of Typhoeus, `--cert` option of libcurl. Related source code:
      # https://github.com/typhoeus/typhoeus/blob/master/lib/typhoeus/easy_factory.rb#L145
      attr_accessor :ssl_ca_cert

      # Set this to enable/disable debugging. When enabled (set to true), HTTP request/response
      # details will be logged with `logger.debug` (see the `logger` attribute).
      # Default to false.
      #
      # @return [true, false]
      attr_accessor :debug

      # Defines the logger used for debugging.
      # Default to `Rails.logger` (when in Rails) or logging to STDOUT.
      #
      # @return [#debug]
      attr_accessor :logger

      # Defines the temporary folder to store downloaded files
      # (for API endpoints that have file response).
      # Default to use `Tempfile`.
      #
      # @return [String]
      attr_accessor :temp_folder_path

      # Defines the headers to be used in HTTP requests of all API calls by default.
      #
      # @return [Hash]
      attr_accessor :default_headers

      # Defaults go in here..
      def initialize
        @format = 'json'
        @scheme = 'http'
        @host = 'petstore.swagger.io'
        @base_path = '/v2'
        @user_agent = "ruby-swagger-#{Swagger::VERSION}"
        @inject_format = false
        @force_ending_format = false

        @default_headers = {
          'Content-Type' => "application/#{@format.downcase}",
          'User-Agent' => @user_agent
        }

        # keys for API key authentication (param-name => api-key)
        @api_key = {}
        @api_key_prefix = {}

        @verify_ssl = true

        @debug = false
        @logger = defined?(Rails) ? Rails.logger : Logger.new(STDOUT)
      end
    end
  end
end
