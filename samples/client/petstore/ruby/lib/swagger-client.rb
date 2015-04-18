# Swagger common files
require 'swagger-client/monkey'
require 'swagger-client/swagger'
require 'swagger-client/swagger/configuration'
require 'swagger-client/swagger/request'
require 'swagger-client/swagger/response'
require 'swagger-client/swagger/version'

# Models
require 'swagger-client/models/user'
require 'swagger-client/models/category'
require 'swagger-client/models/pet'
require 'swagger-client/models/tag'
require 'swagger-client/models/order'

# APIs
require 'swagger-client/api/user_api'
require 'swagger-client/api/pet_api'
require 'swagger-client/api/store_api'

module SwaggerClient
  # Initialize the default configuration
  Swagger.configuration ||= Swagger::Configuration.new
end