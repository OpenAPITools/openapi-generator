local petstore_client = require "petstore.api.pet_api"
local petstore_client_pet = require "petstore.model.pet"

local my_pet_http_api = petstore_client.new("petstore.swagger.io", "/v2", {"http"})

local my_pet = my_pet_http_api:get_pet_by_id(5)
for k,v in pairs(my_pet) do
	print(k,v)
end

local my_new_pet = petstore_client_pet.new("Mr. Barks")
my_pet_http_api:add_pet(my_new_pet)
