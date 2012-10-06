require './lib/swaggering'

# only need to extend if you want special configuration!
class MyApp < Swaggering
  self.configure do |config|
    config.api_version = '0.2' 
  end
end

require './lib/store_api.rb'
require './lib/pet_api.rb'
require './lib/user_api.rb'

