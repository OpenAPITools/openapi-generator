package = "petstore"
version = "1.0.0-1"
source = {
	url = "https://github.com/GIT_USER_ID/GIT_REPO_ID.git"
}

description = {
	summary = "API client genreated by Swagger Codegen",
	detailed = [[
This is a sample server Petstore server.  You can find out more about Swagger at [http://swagger.io](http://swagger.io) or on [irc.freenode.net, #swagger](http://swagger.io/irc/).  For this sample, you can use the api key `special-key` to test the authorization filters.]],
	homepage = "https://github.com/swagger-api/swagger-codegen",
	license = "Unlicense",
	maintainer = "Swagger Codegen contributors",
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
		["petstore.api.pet_api"] = "petstore/api/pet_api.lua";
		["petstore.api.store_api"] = "petstore/api/store_api.lua";
		["petstore.api.user_api"] = "petstore/api/user_api.lua";
		["petstore.model.api_response"] = "petstore/model/api_response.lua";
		["petstore.model.category"] = "petstore/model/category.lua";
		["petstore.model.order"] = "petstore/model/order.lua";
		["petstore.model.pet"] = "petstore/model/pet.lua";
		["petstore.model.tag"] = "petstore/model/tag.lua";
		["petstore.model.user"] = "petstore/model/user.lua";
	}
}
