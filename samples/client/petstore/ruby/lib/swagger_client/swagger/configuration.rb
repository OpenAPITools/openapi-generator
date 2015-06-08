module SwaggerClient
module Swagger
class Configuration
attr_accessor :format, :api_key, :api_key_prefix, :username, :password, :auth_token, :scheme, :host, :base_path, :user_agent, :logger, :inject_format, :force_ending_format, :camelize_params, :user_agent, :verify_ssl

# Defaults go in here..
def initialize
@format = 'json'
@scheme = 'http'
@host = 'petstore.swagger.io'
@base_path = '/v2'
@user_agent = "ruby-swagger-#{Swagger::VERSION}"
@inject_format = false
@force_ending_format = false
@camelize_params = true

# keys for API key authentication (param-name => api-key)
@api_key = {}
# api-key prefix for API key authentication, e.g. "Bearer" (param-name => api-key-prefix)
@api_key_prefix = {}

# whether to verify SSL certificate, default to true
# Note: do NOT set it to false in production code, otherwise you would
#   face multiple types of cryptographic attacks
@verify_ssl = true
end
end
end
end
