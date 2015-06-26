# Swagger common files
require 'petstore/swagger'
require 'petstore/swagger/configuration'
require 'petstore/swagger/api_error'
require 'petstore/swagger/request'
require 'petstore/swagger/response'
require 'petstore/swagger/version'

# Models
require 'petstore/models/base_object'
require 'petstore/models/user'
require 'petstore/models/category'
require 'petstore/models/pet'
require 'petstore/models/tag'
require 'petstore/models/order'

# APIs
require 'petstore/api/user_api'
require 'petstore/api/pet_api'
require 'petstore/api/store_api'

module Petstore
  # Initialize the default configuration
  Swagger.configuration ||= Swagger::Configuration.new
end
