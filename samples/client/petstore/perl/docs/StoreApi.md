# WWW::SwaggerClient::StoreApi

## Load the API package
```perl
use WWW::SwaggerClient::Object::StoreApi;
```

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**delete_order**](StoreApi.md#delete_order) | **DELETE** /store/order/{order_id} | Delete purchase order by ID
[**get_inventory**](StoreApi.md#get_inventory) | **GET** /store/inventory | Returns pet inventories by status
[**get_order_by_id**](StoreApi.md#get_order_by_id) | **GET** /store/order/{order_id} | Find purchase order by ID
[**place_order**](StoreApi.md#place_order) | **POST** /store/order | Place an order for a pet


# **delete_order**
> delete_order(order_id => $order_id)

Delete purchase order by ID

For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors

### Example 
```perl
use Data::Dumper;
use WWW::SwaggerClient::StoreApi;
my $api_instance = WWW::SwaggerClient::StoreApi->new(
);

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
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **get_inventory**
> HASH[string,int] get_inventory()

Returns pet inventories by status

Returns a map of status codes to quantities

### Example 
```perl
use Data::Dumper;
use WWW::SwaggerClient::StoreApi;
my $api_instance = WWW::SwaggerClient::StoreApi->new(

    # Configure API key authorization: api_key
    api_key => {'api_key' => 'YOUR_API_KEY'},
    # uncomment below to setup prefix (e.g. Bearer) for API key, if needed
    #api_key_prefix => {'api_key' => 'Bearer'},
);


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
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **get_order_by_id**
> Order get_order_by_id(order_id => $order_id)

Find purchase order by ID

For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions

### Example 
```perl
use Data::Dumper;
use WWW::SwaggerClient::StoreApi;
my $api_instance = WWW::SwaggerClient::StoreApi->new(
);

my $order_id = 789; # int | ID of pet that needs to be fetched

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
 **order_id** | **int**| ID of pet that needs to be fetched | 

### Return type

[**Order**](Order.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **place_order**
> Order place_order(body => $body)

Place an order for a pet



### Example 
```perl
use Data::Dumper;
use WWW::SwaggerClient::StoreApi;
my $api_instance = WWW::SwaggerClient::StoreApi->new(
);

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
 **body** | [**Order**](Order.md)| order placed for purchasing the pet | 

### Return type

[**Order**](Order.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

