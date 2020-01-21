# load the gem
require 'petstore'

# API client (shared between all the test cases)
API_CLIENT = Petstore::ApiClient.new(Petstore::Configuration.new)

# randomly generate an ID
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
  pet_id
end

# create a random order, return its id
def prepare_store(store_api)
  order_id = 5
  order = Petstore::Order.new("id" => order_id,
          "pet_id" => 123,
          "quantity" => 789,
          "ship_date" => "2015-04-06T23:42:01.678Z",
          "status" => "placed",
          "complete" => false)
  store_api.place_order(order)
  order_id
end

# A random string to tack onto stuff to ensure we're not seeing
# data from a previous test run
RAND = ("a".."z").to_a.sample(8).join

# helper method to serialize object to json string
def serialize_json(o)
  API_CLIENT.object_to_http_body(o)
end

# helper method to deserialize json string back to object
def deserialize_json(s, type)
  headers = { 'Content-Type' => 'application/json' }
  response = double('response', headers: headers, body: s)
  API_CLIENT.deserialize(response, type)
end
