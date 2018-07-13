require 'json'


MyApp.add_route('DELETE', '/v2/store/order/{orderId}', {
  "resourcePath" => "/Store",
  "summary" => "Delete purchase order by ID",
  "nickname" => "delete_order", 
  "responseClass" => "void", 
  "endpoint" => "/store/order/{orderId}", 
  "notes" => "For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors",
  "parameters" => [
    {
      "name" => "order_id",
      "description" => "ID of the order that needs to be deleted",
      "dataType" => "string",
      "paramType" => "path",
    },
    ]}) do
  cross_origin
  # the guts live here

  {"message" => "yes, it worked"}.to_json
end


MyApp.add_route('GET', '/v2/store/inventory', {
  "resourcePath" => "/Store",
  "summary" => "Returns pet inventories by status",
  "nickname" => "get_inventory", 
  "responseClass" => "map[string,int]", 
  "endpoint" => "/store/inventory", 
  "notes" => "Returns a map of status codes to quantities",
  "parameters" => [
    ]}) do
  cross_origin
  # the guts live here

  {"message" => "yes, it worked"}.to_json
end


MyApp.add_route('GET', '/v2/store/order/{orderId}', {
  "resourcePath" => "/Store",
  "summary" => "Find purchase order by ID",
  "nickname" => "get_order_by_id", 
  "responseClass" => "Order", 
  "endpoint" => "/store/order/{orderId}", 
  "notes" => "For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions",
  "parameters" => [
    {
      "name" => "order_id",
      "description" => "ID of pet that needs to be fetched",
      "dataType" => "int",
      "paramType" => "path",
    },
    ]}) do
  cross_origin
  # the guts live here

  {"message" => "yes, it worked"}.to_json
end


MyApp.add_route('POST', '/v2/store/order', {
  "resourcePath" => "/Store",
  "summary" => "Place an order for a pet",
  "nickname" => "place_order", 
  "responseClass" => "Order", 
  "endpoint" => "/store/order", 
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

