# Common files
require 'petstore/api_client'
require 'petstore/api_error'
require 'petstore/version'
require 'petstore/configuration'

# Models
require 'petstore/models/base_object'
require 'petstore/models/user'
require 'petstore/models/category'
require 'petstore/models/pet'
require 'petstore/models/tag'
require 'petstore/models/order'

# APIs
require 'petstore/api/user_api'
require 'petstore/api/store_api'
require 'petstore/api/pet_api'

module Petstore
  class << self
    # Configure sdk using block.
    # Petstore.configure do |config|
    #   config.username = "xxx"
    #   config.password = "xxx"
    # end
    # If no block given, return the configuration singleton instance.
    def configure
      if block_given?
        yield Configuration.instance
      else
        Configuration.instance
      end
    end
  end
end
