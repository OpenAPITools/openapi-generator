# WWW::SwaggerClient::StoreApi

## Load the API package
```perl
use WWW::SwaggerClient::Object::StoreApi;
```

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**delete_order**](StoreApi.md#delete_order) | **DELETE** /store/order/{orderId} | Delete purchase order by ID
[**find_orders_by_status**](StoreApi.md#find_orders_by_status) | **GET** /store/findByStatus | Finds orders by status
[**get_inventory**](StoreApi.md#get_inventory) | **GET** /store/inventory | Returns pet inventories by status
[**get_inventory_in_object**](StoreApi.md#get_inventory_in_object) | **GET** /store/inventory?response&#x3D;arbitrary_object | Fake endpoint to test arbitrary object return by &#39;Get inventory&#39;
[**get_order_by_id**](StoreApi.md#get_order_by_id) | **GET** /store/order/{orderId} | Find purchase order by ID
[**place_order**](StoreApi.md#place_order) | **POST** /store/order | Place an order for a pet


# **delete_order**
> delete_order(order_id => $order_id)

Delete purchase order by ID

For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors

### Example 
```perl
use Data::Dumper;

my $api_instance = WWW::SwaggerClient::StoreApi->new();
my $order_id = 'order_id_example'; # string | ID of the order that needs to be deleted

eval { 
    $api_instance->delete_order(order_id => $order_id);
};
if ($@) {
    warn "Exception when calling StoreApi->delete_order: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **order_id** | **string**| ID of the order that needs to be deleted | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **find_orders_by_status**
> ARRAY[Order] find_orders_by_status(status => $status)

Finds orders by status

A single status value can be provided as a string

### Example 
```perl
use Data::Dumper;

# Configure API key authorization: test_api_client_id
$WWW::SwaggerClient::Configuration::api_key->{'x-test_api_client_id'} = 'YOUR_API_KEY';
# uncomment below to setup prefix (e.g. Bearer) for API key, if needed
#$WWW::SwaggerClient::Configuration::api_key_prefix->{'x-test_api_client_id'} = "Bearer";
# Configure API key authorization: test_api_client_secret
$WWW::SwaggerClient::Configuration::api_key->{'x-test_api_client_secret'} = 'YOUR_API_KEY';
# uncomment below to setup prefix (e.g. Bearer) for API key, if needed
#$WWW::SwaggerClient::Configuration::api_key_prefix->{'x-test_api_client_secret'} = "Bearer";

my $api_instance = WWW::SwaggerClient::StoreApi->new();
my $status = 'status_example'; # string | Status value that needs to be considered for query

eval { 
    my $result = $api_instance->find_orders_by_status(status => $status);
    print Dumper($result);
};
if ($@) {
    warn "Exception when calling StoreApi->find_orders_by_status: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **status** | **string**| Status value that needs to be considered for query | [optional] [default to placed]

### Return type

[**ARRAY[Order]**](Order.md)

### Authorization

[test_api_client_id](../README.md#test_api_client_id), [test_api_client_secret](../README.md#test_api_client_secret)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **get_inventory**
> HASH[string,int] get_inventory()

Returns pet inventories by status

Returns a map of status codes to quantities

### Example 
```perl
use Data::Dumper;

# Configure API key authorization: api_key
$WWW::SwaggerClient::Configuration::api_key->{'api_key'} = 'YOUR_API_KEY';
# uncomment below to setup prefix (e.g. Bearer) for API key, if needed
#$WWW::SwaggerClient::Configuration::api_key_prefix->{'api_key'} = "Bearer";

my $api_instance = WWW::SwaggerClient::StoreApi->new();

eval { 
    my $result = $api_instance->get_inventory();
    print Dumper($result);
};
if ($@) {
    warn "Exception when calling StoreApi->get_inventory: $@\n";
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

**HASH[string,int]**

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **get_inventory_in_object**
> object get_inventory_in_object()

Fake endpoint to test arbitrary object return by 'Get inventory'

Returns an arbitrary object which is actually a map of status codes to quantities

### Example 
```perl
use Data::Dumper;

# Configure API key authorization: api_key
$WWW::SwaggerClient::Configuration::api_key->{'api_key'} = 'YOUR_API_KEY';
# uncomment below to setup prefix (e.g. Bearer) for API key, if needed
#$WWW::SwaggerClient::Configuration::api_key_prefix->{'api_key'} = "Bearer";

my $api_instance = WWW::SwaggerClient::StoreApi->new();

eval { 
    my $result = $api_instance->get_inventory_in_object();
    print Dumper($result);
};
if ($@) {
    warn "Exception when calling StoreApi->get_inventory_in_object: $@\n";
}
```

### Parameters
This endpoint does not need any parameter.

### Return type

**object**

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **get_order_by_id**
> Order get_order_by_id(order_id => $order_id)

Find purchase order by ID

For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions

### Example 
```perl
use Data::Dumper;

# Configure API key authorization: test_api_key_header
$WWW::SwaggerClient::Configuration::api_key->{'test_api_key_header'} = 'YOUR_API_KEY';
# uncomment below to setup prefix (e.g. Bearer) for API key, if needed
#$WWW::SwaggerClient::Configuration::api_key_prefix->{'test_api_key_header'} = "Bearer";
# Configure API key authorization: test_api_key_query
$WWW::SwaggerClient::Configuration::api_key->{'test_api_key_query'} = 'YOUR_API_KEY';
# uncomment below to setup prefix (e.g. Bearer) for API key, if needed
#$WWW::SwaggerClient::Configuration::api_key_prefix->{'test_api_key_query'} = "Bearer";

my $api_instance = WWW::SwaggerClient::StoreApi->new();
my $order_id = 'order_id_example'; # string | ID of pet that needs to be fetched

eval { 
    my $result = $api_instance->get_order_by_id(order_id => $order_id);
    print Dumper($result);
};
if ($@) {
    warn "Exception when calling StoreApi->get_order_by_id: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **order_id** | **string**| ID of pet that needs to be fetched | 

### Return type

[**Order**](Order.md)

### Authorization

[test_api_key_header](../README.md#test_api_key_header), [test_api_key_query](../README.md#test_api_key_query)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **place_order**
> Order place_order(body => $body)

Place an order for a pet



### Example 
```perl
use Data::Dumper;

# Configure API key authorization: test_api_client_id
$WWW::SwaggerClient::Configuration::api_key->{'x-test_api_client_id'} = 'YOUR_API_KEY';
# uncomment below to setup prefix (e.g. Bearer) for API key, if needed
#$WWW::SwaggerClient::Configuration::api_key_prefix->{'x-test_api_client_id'} = "Bearer";
# Configure API key authorization: test_api_client_secret
$WWW::SwaggerClient::Configuration::api_key->{'x-test_api_client_secret'} = 'YOUR_API_KEY';
# uncomment below to setup prefix (e.g. Bearer) for API key, if needed
#$WWW::SwaggerClient::Configuration::api_key_prefix->{'x-test_api_client_secret'} = "Bearer";

my $api_instance = WWW::SwaggerClient::StoreApi->new();
my $body = WWW::SwaggerClient::Object::Order->new(); # Order | order placed for purchasing the pet

eval { 
    my $result = $api_instance->place_order(body => $body);
    print Dumper($result);
};
if ($@) {
    warn "Exception when calling StoreApi->place_order: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Order**](Order.md)| order placed for purchasing the pet | [optional] 

### Return type

[**Order**](Order.md)

### Authorization

[test_api_client_id](../README.md#test_api_client_id), [test_api_client_secret](../README.md#test_api_client_secret)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

