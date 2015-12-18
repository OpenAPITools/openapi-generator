require 'rubygems'
require 'bundler/setup'
require 'petstore'
require 'vcr'
require 'typhoeus'
require 'json'
require 'yaml'
require 'rspec'

RSpec.configure do |config|
  # some (optional) config here
  config.expect_with :rspec do |c|
    c.syntax = :should
  end
  config.mock_with :rspec do |c|
    c.syntax = :should
  end
end


WebMock.allow_net_connect! if defined? WebMock

def help
  puts "\nOh noes! You gotta stuff your swagger credentials in ~/.swagger.yml like so:\n\n"
  puts "api_key: '12345abcdefg'"
  puts "username: 'fumanchu'"
  puts "password: 'kalamazoo'\n\n"
  exit
end

# no longer reading credentials (not used) from file (20150413)
# Parse ~/.swagger.yml for user credentials
#begin
#  CREDENTIALS = YAML::load_file(File.join(ENV['HOME'], ".swagger.yml")).symbolize_keys
#rescue
#  help
#end

API_CLIENT = Petstore::ApiClient.new(Petstore::Configuration.new)

# always delete and then re-create the pet object with 10002
def prepare_pet(pet_api)
  # remove the pet
  pet_api.delete_pet(10002)
  # recreate the pet
  category = Petstore::Category.new('id' => 20002, 'name' => 'category test')
  tag = Petstore::Tag.new('id' => 30002, 'name' => 'tag test')
  pet = Petstore::Pet.new('id' => 10002, 'name' => "RUBY UNIT TESTING", 'photo_urls' => 'photo url',
                               'category' => category, 'tags' => [tag], 'status' => 'pending')

  pet_api.add_pet(:'body'=> pet)
end

# always delete and then re-create the store order
def prepare_store(store_api)
  order = Petstore::Order.new("id" => 10002,
		  "petId" => 10002,
		  "quantity" => 789,
		  "shipDate" => "2015-04-06T23:42:01.678Z",
		  "status" => "placed",
		  "complete" => false)
  store_api.place_order(:body => order)
end

# A random string to tack onto stuff to ensure we're not seeing
# data from a previous test run
RAND = ("a".."z").to_a.sample(8).join
