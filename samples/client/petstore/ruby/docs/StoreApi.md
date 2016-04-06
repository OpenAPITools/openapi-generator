# Petstore::StoreApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**delete_order**](StoreApi.md#delete_order) | **DELETE** /store/order/{orderId} | Delete purchase order by ID
[**find_orders_by_status**](StoreApi.md#find_orders_by_status) | **GET** /store/findByStatus | Finds orders by status
[**get_inventory**](StoreApi.md#get_inventory) | **GET** /store/inventory | Returns pet inventories by status
[**get_inventory_in_object**](StoreApi.md#get_inventory_in_object) | **GET** /store/inventory?response=arbitrary_object | Fake endpoint to test arbitrary object return by &#39;Get inventory&#39;
[**get_order_by_id**](StoreApi.md#get_order_by_id) | **GET** /store/order/{orderId} | Find purchase order by ID
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

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml



# **find_orders_by_status**
> Array&lt;Order&gt; find_orders_by_status(opts)

Finds orders by status

A single status value can be provided as a string

### Example
```ruby
# load the gem
require 'petstore'
# setup authorization 
Petstore.configure do |config|
  # Configure API key authorization: test_api_client_id
  config.api_key['x-test_api_client_id'] = 'YOUR API KEY'
  # Uncomment the following line to set a prefix for the API key, e.g. 'BEARER' (defaults to nil)
  #config.api_key_prefix['x-test_api_client_id'] = 'BEARER'

  # Configure API key authorization: test_api_client_secret
  config.api_key['x-test_api_client_secret'] = 'YOUR API KEY'
  # Uncomment the following line to set a prefix for the API key, e.g. 'BEARER' (defaults to nil)
  #config.api_key_prefix['x-test_api_client_secret'] = 'BEARER'
end

api_instance = Petstore::StoreApi.new

opts = { 
  status: "placed" # String | Status value that needs to be considered for query
}

begin
  #Finds orders by status
  result = api_instance.find_orders_by_status(opts)
  p result
rescue Petstore::ApiError => e
  puts "Exception when calling StoreApi->find_orders_by_status: #{e}"
end
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **status** | **String**| Status value that needs to be considered for query | [optional] [default to placed]

### Return type

[**Array&lt;Order&gt;**](Order.md)

### Authorization

[test_api_client_id](../README.md#test_api_client_id), [test_api_client_secret](../README.md#test_api_client_secret)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml



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
  # Uncomment the following line to set a prefix for the API key, e.g. 'BEARER' (defaults to nil)
  #config.api_key_prefix['api_key'] = 'BEARER'
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

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml



# **get_inventory_in_object**
> Object get_inventory_in_object

Fake endpoint to test arbitrary object return by 'Get inventory'

Returns an arbitrary object which is actually a map of status codes to quantities

### Example
```ruby
# load the gem
require 'petstore'
# setup authorization 
Petstore.configure do |config|
  # Configure API key authorization: api_key
  config.api_key['api_key'] = 'YOUR API KEY'
  # Uncomment the following line to set a prefix for the API key, e.g. 'BEARER' (defaults to nil)
  #config.api_key_prefix['api_key'] = 'BEARER'
end

api_instance = Petstore::StoreApi.new

begin
  #Fake endpoint to test arbitrary object return by 'Get inventory'
  result = api_instance.get_inventory_in_object
  p result
rescue Petstore::ApiError => e
  puts "Exception when calling StoreApi->get_inventory_in_object: #{e}"
end
```

### Parameters
This endpoint does not need any parameter.

### Return type

**Object**

### Authorization

[api_key](../README.md#api_key)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml



# **get_order_by_id**
> Order get_order_by_id(order_id)

Find purchase order by ID

For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions

### Example
```ruby
# load the gem
require 'petstore'
# setup authorization 
Petstore.configure do |config|
  # Configure API key authorization: test_api_key_header
  config.api_key['test_api_key_header'] = 'YOUR API KEY'
  # Uncomment the following line to set a prefix for the API key, e.g. 'BEARER' (defaults to nil)
  #config.api_key_prefix['test_api_key_header'] = 'BEARER'

  # Configure API key authorization: test_api_key_query
  config.api_key['test_api_key_query'] = 'YOUR API KEY'
  # Uncomment the following line to set a prefix for the API key, e.g. 'BEARER' (defaults to nil)
  #config.api_key_prefix['test_api_key_query'] = 'BEARER'
end

api_instance = Petstore::StoreApi.new

order_id = "order_id_example" # String | ID of pet that needs to be fetched


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
 **order_id** | **String**| ID of pet that needs to be fetched | 

### Return type

[**Order**](Order.md)

### Authorization

[test_api_key_header](../README.md#test_api_key_header), [test_api_key_query](../README.md#test_api_key_query)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml



# **place_order**
> Order place_order(opts)

Place an order for a pet



### Example
```ruby
# load the gem
require 'petstore'
# setup authorization 
Petstore.configure do |config|
  # Configure API key authorization: test_api_client_id
  config.api_key['x-test_api_client_id'] = 'YOUR API KEY'
  # Uncomment the following line to set a prefix for the API key, e.g. 'BEARER' (defaults to nil)
  #config.api_key_prefix['x-test_api_client_id'] = 'BEARER'

  # Configure API key authorization: test_api_client_secret
  config.api_key['x-test_api_client_secret'] = 'YOUR API KEY'
  # Uncomment the following line to set a prefix for the API key, e.g. 'BEARER' (defaults to nil)
  #config.api_key_prefix['x-test_api_client_secret'] = 'BEARER'
end

api_instance = Petstore::StoreApi.new

opts = { 
  body: Petstore::Order.new # Order | order placed for purchasing the pet
}

begin
  #Place an order for a pet
  result = api_instance.place_order(opts)
  p result
rescue Petstore::ApiError => e
  puts "Exception when calling StoreApi->place_order: #{e}"
end
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Order**](Order.md)| order placed for purchasing the pet | [optional] 

### Return type

[**Order**](Order.md)

### Authorization

[test_api_client_id](../README.md#test_api_client_id), [test_api_client_secret](../README.md#test_api_client_secret)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml



