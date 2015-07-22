# Common files
require 'petstore/api_client'
require 'petstore/api_error'
require 'petstore/request'
require 'petstore/response'
require 'petstore/version'

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
end
