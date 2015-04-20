# Swagger common files
require 'swagger_client/monkey'
require 'swagger_client/swagger'
require 'swagger_client/swagger/configuration'
require 'swagger_client/swagger/request'
require 'swagger_client/swagger/response'
require 'swagger_client/swagger/version'

# Models
require 'swagger_client/models/user'
require 'swagger_client/models/category'
require 'swagger_client/models/pet'
require 'swagger_client/models/tag'
require 'swagger_client/models/order'

# APIs
require 'swagger_client/api/user_api'
require 'swagger_client/api/pet_api'
require 'swagger_client/api/store_api'

module SwaggerClient
  # Initialize the default configuration
  Swagger.configuration ||= Swagger::Configuration.new
end