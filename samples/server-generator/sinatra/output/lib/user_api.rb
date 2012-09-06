require 'json'

MyApp.add_route('post', '/createWithArray', {
  "resourcePath" => "/user",
  "summary" => "Creates list of users with given input array",
  "nickname" => "createUsersWithArrayInput", 
  "responseClass" => "void", 
  "endpoint" => "/createWithArray", 
  "notes" => "",
  "parameters" => [
    {
      "name" => "body",
      "description" => "List of user object",
      "dataType" => "Array[User]",
      "paramType" => "body",
    }
    ]}) do
  cross_origin
  # the guts live here

  {"message" => "yes, it worked"}.to_json
end

MyApp.add_route('post', '/', {
  "resourcePath" => "/user",
  "summary" => "Create user",
  "nickname" => "createUser", 
  "responseClass" => "void", 
  "endpoint" => "/", 
  "notes" => "This can only be done by the logged in user.",
  "parameters" => [
    {
      "name" => "body",
      "description" => "Created user object",
      "dataType" => "User",
      "paramType" => "body",
    }
    ]}) do
  cross_origin
  # the guts live here

  {"message" => "yes, it worked"}.to_json
end

MyApp.add_route('post', '/createWithList', {
  "resourcePath" => "/user",
  "summary" => "Creates list of users with given list input",
  "nickname" => "createUsersWithListInput", 
  "responseClass" => "void", 
  "endpoint" => "/createWithList", 
  "notes" => "",
  "parameters" => [
    {
      "name" => "body",
      "description" => "List of user object",
      "dataType" => "List[User]",
      "paramType" => "body",
    }
    ]}) do
  cross_origin
  # the guts live here

  {"message" => "yes, it worked"}.to_json
end

MyApp.add_route('put', '/:username', {
  "resourcePath" => "/user",
  "summary" => "Updated user",
  "nickname" => "updateUser", 
  "responseClass" => "void", 
  "endpoint" => "/:username", 
  "notes" => "This can only be done by the logged in user.",
  "parameters" => [
    {
      "name" => "username",
      "description" => "name that need to be deleted",
      "dataType" => "string",
      "paramType" => "path",
    },
    {
      "name" => "body",
      "description" => "Updated user object",
      "dataType" => "User",
      "paramType" => "body",
    }
    ]}) do
  cross_origin
  # the guts live here

  {"message" => "yes, it worked"}.to_json
end

MyApp.add_route('delete', '/:username', {
  "resourcePath" => "/user",
  "summary" => "Delete user",
  "nickname" => "deleteUser", 
  "responseClass" => "void", 
  "endpoint" => "/:username", 
  "notes" => "This can only be done by the logged in user.",
  "parameters" => [
    {
      "name" => "username",
      "description" => "The name that needs to be deleted",
      "dataType" => "string",
      "paramType" => "path",
    },
    ]}) do
  cross_origin
  # the guts live here

  {"message" => "yes, it worked"}.to_json
end

MyApp.add_route('get', '/:username', {
  "resourcePath" => "/user",
  "summary" => "Get user by user name",
  "nickname" => "getUserByName", 
  "responseClass" => "User", 
  "endpoint" => "/:username", 
  "notes" => "",
  "parameters" => [
    {
      "name" => "username",
      "description" => "The name that needs to be fetched. Use user1 for testing.",
      "dataType" => "string",
      "paramType" => "path",
    },
    ]}) do
  cross_origin
  # the guts live here

  {"message" => "yes, it worked"}.to_json
end

MyApp.add_route('get', '/login', {
  "resourcePath" => "/user",
  "summary" => "Logs user into the system",
  "nickname" => "loginUser", 
  "responseClass" => "string", 
  "endpoint" => "/login", 
  "notes" => "",
  "parameters" => [
    {
      "name" => "username",
      "description" => "The user name for login",
      "dataType" => "string",
      "paramType" => "query",
      "allowMultiple" => false,
      "allowableValues" => "",
      },
    {
      "name" => "password",
      "description" => "The password for login in clear text",
      "dataType" => "string",
      "paramType" => "query",
      "allowMultiple" => false,
      "allowableValues" => "",
      },
    ]}) do
  cross_origin
  # the guts live here

  {"message" => "yes, it worked"}.to_json
end

MyApp.add_route('get', '/logout', {
  "resourcePath" => "/user",
  "summary" => "Logs out current logged in user session",
  "nickname" => "logoutUser", 
  "responseClass" => "void", 
  "endpoint" => "/logout", 
  "notes" => "",
  "parameters" => [
    ]}) do
  cross_origin
  # the guts live here

  {"message" => "yes, it worked"}.to_json
end


