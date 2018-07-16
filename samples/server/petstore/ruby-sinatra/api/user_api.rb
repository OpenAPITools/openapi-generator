require 'json'


MyApp.add_route('POST', '/v2/user', {
  "resourcePath" => "/User",
  "summary" => "Create user",
  "nickname" => "create_user", 
  "responseClass" => "void",
  "endpoint" => "/user", 
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


MyApp.add_route('POST', '/v2/user/createWithArray', {
  "resourcePath" => "/User",
  "summary" => "Creates list of users with given input array",
  "nickname" => "create_users_with_array_input", 
  "responseClass" => "void",
  "endpoint" => "/user/createWithArray", 
  "notes" => "",
  "parameters" => [
    {
      "name" => "body",
      "description" => "List of user object",
      "dataType" => "Array<User>",
      "paramType" => "body",
    }
    ]}) do
  cross_origin
  # the guts live here

  {"message" => "yes, it worked"}.to_json
end


MyApp.add_route('POST', '/v2/user/createWithList', {
  "resourcePath" => "/User",
  "summary" => "Creates list of users with given input array",
  "nickname" => "create_users_with_list_input", 
  "responseClass" => "void",
  "endpoint" => "/user/createWithList", 
  "notes" => "",
  "parameters" => [
    {
      "name" => "body",
      "description" => "List of user object",
      "dataType" => "Array<User>",
      "paramType" => "body",
    }
    ]}) do
  cross_origin
  # the guts live here

  {"message" => "yes, it worked"}.to_json
end


MyApp.add_route('DELETE', '/v2/user/{username}', {
  "resourcePath" => "/User",
  "summary" => "Delete user",
  "nickname" => "delete_user", 
  "responseClass" => "void",
  "endpoint" => "/user/{username}", 
  "notes" => "This can only be done by the logged in user.",
  "parameters" => [
    {
      "name" => "username",
      "description" => "The name that needs to be deleted",
      "dataType" => "String",
      "paramType" => "path",
    },
    ]}) do
  cross_origin
  # the guts live here

  {"message" => "yes, it worked"}.to_json
end


MyApp.add_route('GET', '/v2/user/{username}', {
  "resourcePath" => "/User",
  "summary" => "Get user by user name",
  "nickname" => "get_user_by_name", 
  "responseClass" => "User",
  "endpoint" => "/user/{username}", 
  "notes" => "",
  "parameters" => [
    {
      "name" => "username",
      "description" => "The name that needs to be fetched. Use user1 for testing.",
      "dataType" => "String",
      "paramType" => "path",
    },
    ]}) do
  cross_origin
  # the guts live here

  {"message" => "yes, it worked"}.to_json
end


MyApp.add_route('GET', '/v2/user/login', {
  "resourcePath" => "/User",
  "summary" => "Logs user into the system",
  "nickname" => "login_user", 
  "responseClass" => "String",
  "endpoint" => "/user/login", 
  "notes" => "",
  "parameters" => [
    {
      "name" => "username",
      "description" => "The user name for login",
      "dataType" => "String",
      "allowableValues" => "",
      "paramType" => "query",
    },
    {
      "name" => "password",
      "description" => "The password for login in clear text",
      "dataType" => "String",
      "allowableValues" => "",
      "paramType" => "query",
    },
    ]}) do
  cross_origin
  # the guts live here

  {"message" => "yes, it worked"}.to_json
end


MyApp.add_route('GET', '/v2/user/logout', {
  "resourcePath" => "/User",
  "summary" => "Logs out current logged in user session",
  "nickname" => "logout_user", 
  "responseClass" => "void",
  "endpoint" => "/user/logout", 
  "notes" => "",
  "parameters" => [
    ]}) do
  cross_origin
  # the guts live here

  {"message" => "yes, it worked"}.to_json
end


MyApp.add_route('PUT', '/v2/user/{username}', {
  "resourcePath" => "/User",
  "summary" => "Updated user",
  "nickname" => "update_user", 
  "responseClass" => "void",
  "endpoint" => "/user/{username}", 
  "notes" => "This can only be done by the logged in user.",
  "parameters" => [
    {
      "name" => "username",
      "description" => "name that need to be deleted",
      "dataType" => "String",
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

