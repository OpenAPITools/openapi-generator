require 'json'

MyApp.add_route('get', '/order/:orderId', {
  "resourcePath" => "/store",
  "summary" => "Find purchase order by ID",
  "nickname" => "getOrderById", 
  "responseClass" => "Order", 
  "endpoint" => "/order/:orderId", 
  "notes" => "For valid response try integer IDs with value <= 5. Anything above 5 or nonintegers will generate API errors",
  "parameters" => [
    {
      "name" => "orderId",
      "description" => "ID of pet that needs to be fetched",
      "dataType" => "string",
      "paramType" => "path",
    },
    ]}) do
  cross_origin
  # the guts live here

  {"message" => "yes, it worked"}.to_json
end

MyApp.add_route('delete', '/order/:orderId', {
  "resourcePath" => "/store",
  "summary" => "Delete purchase order by ID",
  "nickname" => "deleteOrder", 
  "responseClass" => "void", 
  "endpoint" => "/order/:orderId", 
  "notes" => "For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors",
  "parameters" => [
    {
      "name" => "orderId",
      "description" => "ID of the order that needs to be deleted",
      "dataType" => "string",
      "paramType" => "path",
    },
    ]}) do
  cross_origin
  # the guts live here

  {"message" => "yes, it worked"}.to_json
end

MyApp.add_route('post', '/order', {
  "resourcePath" => "/store",
  "summary" => "Place an order for a pet",
  "nickname" => "placeOrder", 
  "responseClass" => "void", 
  "endpoint" => "/order", 
  "notes" => "",
  "parameters" => [
    {
      "name" => "body",
      "description" => "order placed for purchasing the pet",
      "dataType" => "Order",
      "paramType" => "body",
    }
    ]}) do
  cross_origin
  # the guts live here

  {"message" => "yes, it worked"}.to_json
end


