require 'json'

MyApp.add_route('get', '/:petId', {
  "resourcePath" => "/pet",
  "summary" => "Find pet by ID",
  "nickname" => "getPetById", 
  "responseClass" => "Pet", 
  "endpoint" => "/:petId", 
  "notes" => "Returns a pet based on ID",
  "parameters" => [
    {
      "name" => "petId",
      "description" => "ID of pet that needs to be fetched",
      "dataType" => "string",
      "paramType" => "path",
    },
    ]}) do
  cross_origin
  # the guts live here

  {"message" => "yes, it worked"}.to_json
end

MyApp.add_route('post', '/', {
  "resourcePath" => "/pet",
  "summary" => "Add a new pet to the store",
  "nickname" => "addPet", 
  "responseClass" => "void", 
  "endpoint" => "/", 
  "notes" => "",
  "parameters" => [
    {
      "name" => "body",
      "description" => "Pet object that needs to be added to the store",
      "dataType" => "Pet",
      "paramType" => "body",
    }
    ]}) do
  cross_origin
  # the guts live here

  {"message" => "yes, it worked"}.to_json
end

MyApp.add_route('put', '/', {
  "resourcePath" => "/pet",
  "summary" => "Update an existing pet",
  "nickname" => "updatePet", 
  "responseClass" => "void", 
  "endpoint" => "/", 
  "notes" => "",
  "parameters" => [
    {
      "name" => "body",
      "description" => "Pet object that needs to be updated in the store",
      "dataType" => "Pet",
      "paramType" => "body",
    }
    ]}) do
  cross_origin
  # the guts live here

  {"message" => "yes, it worked"}.to_json
end

MyApp.add_route('get', '/findByStatus', {
  "resourcePath" => "/pet",
  "summary" => "Finds Pets by status",
  "nickname" => "findPetsByStatus", 
  "responseClass" => "List[Pet]", 
  "endpoint" => "/findByStatus", 
  "notes" => "Multiple status values can be provided with comma seperated strings",
  "parameters" => [
    {
      "name" => "status",
      "description" => "Status values that need to be considered for filter",
      "dataType" => "string",
      "paramType" => "query",
      "allowMultiple" => true,
      "allowableValues" => "LIST[available,pending,sold]",
      "defaultValue" => "available"},
    ]}) do
  cross_origin
  # the guts live here

  {"message" => "yes, it worked"}.to_json
end

MyApp.add_route('get', '/findByTags', {
  "resourcePath" => "/pet",
  "summary" => "Finds Pets by tags",
  "nickname" => "findPetsByTags", 
  "responseClass" => "List[Pet]", 
  "endpoint" => "/findByTags", 
  "notes" => "Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.",
  "parameters" => [
    {
      "name" => "tags",
      "description" => "Tags to filter by",
      "dataType" => "string",
      "paramType" => "query",
      "allowMultiple" => true,
      "allowableValues" => "",
      },
    ]}) do
  cross_origin
  # the guts live here

  {"message" => "yes, it worked"}.to_json
end


