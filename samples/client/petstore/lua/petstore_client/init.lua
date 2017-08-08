local http_request = require "http.request"
local http_util = require "http.util"
local dkjson = require "dkjson"

local petstore_client_pet = require "petstore_client.pet"

-- API class
local pet_http_api = {}
local pet_http_api_mt = {
	__name = "pet-http-api";
	__index = pet_http_api;
}

local function new_pet_http_api(host, basePath, schemes)
	local schemes_map = {}
	for _,v in ipairs(schemes) do
		schemes_map[v] = v
	end
	local default_scheme = schemes_map.https or schemes_map.http
	return setmetatable({
		host = host;
		basePath = basePath;
		schemes = schemes_map;
		default_scheme = default_scheme;
	}, pet_http_api_mt)
end

function pet_http_api:getPetById(petId)
	local req = http_request.new_from_uri({
		scheme = self.default_scheme;
		host = self.host;
		path = string.format("%s/pet/%d", self.basePath, petId);
	})
	req.headers:upsert("accept", "application/json")
	local headers, stream, errno = req:go()
	if not headers then
		return nil, stream, errno
	end
	local http_status = headers:get(":status")
	if http_status == "200" then
		local body, err, errno2 = stream:get_body_as_string()
		if not body then
			return nil, err, errno2
		end
		stream:shutdown()
		local result, _, err3 = dkjson.decode(body)
		if result == nil then
			return nil, err3
		end
		return petstore_client_pet.cast(result)
	elseif http_status == "400" then
		stream:shutdown()
		return nil, "Invalid ID supplied"
	elseif http_status == "404" then
		stream:shutdown()
		return nil, "Pet not found"
	else
		stream:shutdown()
		return nil, "Unexpected response status code"
	end
end

function pet_http_api:updatePetWithForm(petId, name, status)
	local req = http_request.new_from_uri({
		scheme = self.default_scheme;
		host = self.host;
		path = string.format("%s/pet/%d", self.basePath, petId);
	})
	req.headers:upsert(":method", "POST")
	req.headers:upsert("content-type", "application/x-www-form-urlencoded")
	req:set_body(http_util.dict_to_query({
		name = name;
		status = status;
	}))
	req.headers:upsert("accept", "application/json")
	local headers, stream, errno = req:go()
	if not headers then
		return nil, stream, errno
	end
	local http_status = headers:get(":status")
	if http_status == "405" then
		stream:shutdown()
		return nil, "Invalid input"
	else
		-- TODO: this should handle 200... but it's not in the spec?
		stream:shutdown()
		return nil, "Unexpected response status code"
	end
end

function pet_http_api:addPet(pet)
	local req = http_request.new_from_uri({
		scheme = self.default_scheme;
		host = self.host;
		path = string.format("%s/pet", self.basePath);
	})
	req.headers:upsert(":method", "POST")
	req.headers:upsert("content-type", "application/json")
	req:set_body(dkjson.encode(pet))
	req.headers:upsert("accept", "application/json")
	local headers, stream, errno = req:go()
	if not headers then
		return nil, stream, errno
	end
	local http_status = headers:get(":status")
	if http_status == "405" then
		stream:shutdown()
		return nil, "Invalid input"
	else
		-- TODO: this should handle 200... but it's not in the spec?
		stream:shutdown()
		return nil, "Unexpected response status code"
	end
end

return {
	new = new_pet_http_api;
}
