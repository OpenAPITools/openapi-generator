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

def random_id
  rand(1000000) + 20000
end

# create a random pet, return its id
def prepare_pet(pet_api)
  pet_id = random_id
  category = Petstore::Category.new('id' => 20002, 'name' => 'category test')
  tag = Petstore::Tag.new('id' => 30002, 'name' => 'tag test')
  pet = Petstore::Pet.new('id' => pet_id, 'name' => "RUBY UNIT TESTING", 'photo_urls' => 'photo url',
                          'category' => category, 'tags' => [tag], 'status' => 'pending')
  pet_api.add_pet(pet)
  return pet_id
end

# create a random order, return its id
def prepare_store(store_api)
  order_id = 5
  order = Petstore::Order.new("id" => order_id,
		  "petId" => 123,
		  "quantity" => 789,
		  "shipDate" => "2015-04-06T23:42:01.678Z",
		  "status" => "placed",
		  "complete" => false)
  store_api.place_order(order)
  return order_id
end

# A random string to tack onto stuff to ensure we're not seeing
# data from a previous test run
RAND = ("a".."z").to_a.sample(8).join
