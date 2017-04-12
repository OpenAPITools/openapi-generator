# Petstore::StoreApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**delete_order**](StoreApi.md#delete_order) | **DELETE** /store/order/{order_id} | Delete purchase order by ID
[**get_inventory**](StoreApi.md#get_inventory) | **GET** /store/inventory | Returns pet inventories by status
[**get_order_by_id**](StoreApi.md#get_order_by_id) | **GET** /store/order/{order_id} | Find purchase order by ID
[**place_order**](StoreApi.md#place_order) | **POST** /store/order | Place an order for a pet


# **delete_order**
> delete_order(order_id)

Delete purchase order by ID

For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors

### Example
```ruby
# load the gem
require 'petstore'

api_instance = Petstore::StoreApi.new

order_id = "order_id_example" # String | ID of the order that needs to be deleted


begin
  #Delete purchase order by ID
  api_instance.delete_order(order_id)
rescue Petstore::ApiError => e
  puts "Exception when calling StoreApi->delete_order: #{e}"
end
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **order_id** | **String**| ID of the order that needs to be deleted | 

### Return type

nil (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json



# **get_inventory**
> Hash&lt;String, Integer&gt; get_inventory

Returns pet inventories by status

Returns a map of status codes to quantities

### Example
```ruby
# load the gem
require 'petstore'
# setup authorization
Petstore.configure do |config|
  # Configure API key authorization: api_key
  config.api_key['api_key'] = 'YOUR API KEY'
  # Uncomment the following line to set a prefix for the API key, e.g. 'Bearer' (defaults to nil)
  #config.api_key_prefix['api_key'] = 'Bearer'
end

api_instance = Petstore::StoreApi.new

begin
  #Returns pet inventories by status
  result = api_instance.get_inventory
  p result
rescue Petstore::ApiError => e
  puts "Exception when calling StoreApi->get_inventory: #{e}"
end
```

### Parameters
This endpoint does not need any parameter.

### Return type

**Hash&lt;String, Integer&gt;**

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json



# **get_order_by_id**
> Order get_order_by_id(order_id)

Find purchase order by ID

For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions

### Example
```ruby
# load the gem
require 'petstore'

api_instance = Petstore::StoreApi.new

order_id = 789 # Integer | ID of pet that needs to be fetched


begin
  #Find purchase order by ID
  result = api_instance.get_order_by_id(order_id)
  p result
rescue Petstore::ApiError => e
  puts "Exception when calling StoreApi->get_order_by_id: #{e}"
end
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **order_id** | **Integer**| ID of pet that needs to be fetched | 

### Return type

[**Order**](Order.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json



# **place_order**
> Order place_order(body)

Place an order for a pet



### Example
```ruby
# load the gem
require 'petstore'

api_instance = Petstore::StoreApi.new

body = Petstore::Order.new # Order | order placed for purchasing the pet


begin
  #Place an order for a pet
  result = api_instance.place_order(body)
  p result
rescue Petstore::ApiError => e
  puts "Exception when calling StoreApi->place_order: #{e}"
end
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Order**](Order.md)| order placed for purchasing the pet | 

### Return type

[**Order**](Order.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json



