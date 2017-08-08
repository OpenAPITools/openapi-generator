local petstore_client = require "petstore_client"
local petstore_client_pet = require "petstore_client.pet"

local my_pet_http_api = petstore_client.new("petstore.swagger.io", "/v2", {"http"})

local my_pet = assert(my_pet_http_api:getPetById(4))
for k,v in pairs(my_pet) do
	print(k,v)
end

local my_new_pet = petstore_client_pet.new("Mr. Barks")
assert(my_pet_http_api:addPet(my_new_pet))
