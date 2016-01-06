# Common files
require 'petstore/api_client'
require 'petstore/api_error'
require 'petstore/version'
require 'petstore/configuration'

# Models
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
  class << self
    # Customize default settings for the SDK using block.
    #   Petstore.configure do |config|
    #     config.username = "xxx"
    #     config.password = "xxx"
    #   end
    # If no block given, return the default Configuration object.
    def configure
      if block_given?
        yield(Configuration.default)
      else
        Configuration.default
      end
    end
  end
end
