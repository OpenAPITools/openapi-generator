require 'json'


MyApp.add_route('POST', '/v2/pets', {
  "resourcePath" => "/Pet",
  "summary" => "Add a new pet to the store",
  "nickname" => "add_pet", 
  "responseClass" => "void", 
  "endpoint" => "/pets", 
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


MyApp.add_route('DELETE', '/v2/pets/{petId}', {
  "resourcePath" => "/Pet",
  "summary" => "Deletes a pet",
  "nickname" => "delete_pet", 
  "responseClass" => "void", 
  "endpoint" => "/pets/{petId}", 
  "notes" => "",
  "parameters" => [
    
    
    {
      "name" => "pet_id",
      "description" => "Pet id to delete",
      "dataType" => "int",
      "paramType" => "path",
    },
    
    
    {
      "name" => "api_key",
      "description" => "",
      "dataType" => "string",
      "paramType" => "header",
    },
    
    
    ]}) do
  cross_origin
  # the guts live here

  {"message" => "yes, it worked"}.to_json
end


MyApp.add_route('GET', '/v2/pets/findByStatus', {
  "resourcePath" => "/Pet",
  "summary" => "Finds Pets by status",
  "nickname" => "find_pets_by_status", 
  "responseClass" => "array[Pet]", 
  "endpoint" => "/pets/findByStatus", 
  "notes" => "Multiple status values can be provided with comma seperated strings",
  "parameters" => [
    
    {
      "name" => "status",
      "description" => "Status values that need to be considered for filter",
      "dataType" => "array[string]",
      "paramType" => "query",
      "collectionFormat" => "multi",
      "allowableValues" => "",
      
    },
    
    
    
    
    ]}) do
  cross_origin
  # the guts live here

  {"message" => "yes, it worked"}.to_json
end


MyApp.add_route('GET', '/v2/pets/findByTags', {
  "resourcePath" => "/Pet",
  "summary" => "Finds Pets by tags",
  "nickname" => "find_pets_by_tags", 
  "responseClass" => "array[Pet]", 
  "endpoint" => "/pets/findByTags", 
  "notes" => "Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.",
  "parameters" => [
    
    {
      "name" => "tags",
      "description" => "Tags to filter by",
      "dataType" => "array[string]",
      "paramType" => "query",
      "collectionFormat" => "multi",
      "allowableValues" => "",
      
    },
    
    
    
    
    ]}) do
  cross_origin
  # the guts live here

  {"message" => "yes, it worked"}.to_json
end


MyApp.add_route('GET', '/v2/pets/{petId}', {
  "resourcePath" => "/Pet",
  "summary" => "Find pet by ID",
  "nickname" => "get_pet_by_id", 
  "responseClass" => "Pet", 
  "endpoint" => "/pets/{petId}", 
  "notes" => "Returns a pet when ID < 10.  ID > 10 or nonintegers will simulate API error conditions",
  "parameters" => [
    
    
    {
      "name" => "pet_id",
      "description" => "ID of pet that needs to be fetched",
      "dataType" => "int",
      "paramType" => "path",
    },
    
    
    
    ]}) do
  cross_origin
  # the guts live here

  {"message" => "yes, it worked"}.to_json
end


MyApp.add_route('PUT', '/v2/pets', {
  "resourcePath" => "/Pet",
  "summary" => "Update an existing pet",
  "nickname" => "update_pet", 
  "responseClass" => "void", 
  "endpoint" => "/pets", 
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


MyApp.add_route('POST', '/v2/pets/{petId}', {
  "resourcePath" => "/Pet",
  "summary" => "Updates a pet in the store with form data",
  "nickname" => "update_pet_with_form", 
  "responseClass" => "void", 
  "endpoint" => "/pets/{petId}", 
  "notes" => "",
  "parameters" => [
    
    
    {
      "name" => "pet_id",
      "description" => "ID of pet that needs to be updated",
      "dataType" => "string",
      "paramType" => "path",
    },
    
    
    
    ]}) do
  cross_origin
  # the guts live here

  {"message" => "yes, it worked"}.to_json
end

