package = "openapiclient"
version = "1.0.0-1"
source = {
	url = "https://github.com/GIT_USER_ID/GIT_REPO_ID.git"
}

description = {
	summary = "API client genreated by OpenAPI Generator",
	detailed = [[
This is a sample server Petstore server. For this sample, you can use the api key `special-key` to test the authorization filters.]],
	homepage = "https://openapi-generator.tech",
	license = "Unlicense",
	maintainer = "OpenAPI Generator contributors",
}

dependencies = {
	"lua >= 5.2",
	"http",
	"dkjson",
	"basexx"
}

build = {
	type = "builtin",
	modules = {
		["openapiclient.api.pet_api"] = "openapiclient/api/pet_api.lua";
		["openapiclient.api.store_api"] = "openapiclient/api/store_api.lua";
		["openapiclient.api.user_api"] = "openapiclient/api/user_api.lua";
		["openapiclient.model.api_response"] = "openapiclient/model/api_response.lua";
		["openapiclient.model.category"] = "openapiclient/model/category.lua";
		["openapiclient.model.order"] = "openapiclient/model/order.lua";
		["openapiclient.model.pet"] = "openapiclient/model/pet.lua";
		["openapiclient.model.tag"] = "openapiclient/model/tag.lua";
		["openapiclient.model.user"] = "openapiclient/model/user.lua";
	}
}
