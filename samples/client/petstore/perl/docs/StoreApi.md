# WWW::SwaggerClient::StoreApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**find_orders_by_status**](StoreApi.md#find_orders_by_status) | **GET** /store/findByStatus | Finds orders by status
[**get_inventory**](StoreApi.md#get_inventory) | **GET** /store/inventory | Returns pet inventories by status
[**get_inventory_in_object**](StoreApi.md#get_inventory_in_object) | **GET** /store/inventory?response=arbitrary_object | Fake endpoint to test arbitrary object return by &#39;Get inventory&#39;
[**place_order**](StoreApi.md#place_order) | **POST** /store/order | Place an order for a pet
[**get_order_by_id**](StoreApi.md#get_order_by_id) | **GET** /store/order/{orderId} | Find purchase order by ID
[**delete_order**](StoreApi.md#delete_order) | **DELETE** /store/order/{orderId} | Delete purchase order by ID


# **find_orders_by_status**
> find_orders_by_status(status => $status)

Finds orders by status

A single status value can be provided as a string

### Sample 
```perl
my $api = WWW::SwaggerClient::StoreApi->new();
my $status = 'status_example'; # [string] Status value that needs to be considered for query

eval { 
    my $result = $api->find_orders_by_status(status => $status);
};
if ($@) {
    warn "Exception when calling find_orders_by_status: $@\n";
}
```

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **status** | [**string**](docs/.md)| Status value that needs to be considered for query | [optional] [default to placed]

### Return type

[**ARRAY[Order]**](Order.md)

### HTTP headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

### Authentication scheme

test_api_client_idtest_api_client_secret





# **get_inventory**
> get_inventory()

Returns pet inventories by status

Returns a map of status codes to quantities

### Sample 
```perl
my $api = WWW::SwaggerClient::StoreApi->new();

eval { 
    my $result = $api->get_inventory();
};
if ($@) {
    warn "Exception when calling get_inventory: $@\n";
}
```

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------

### Return type

[**HASH[string,int]**](HASH.md)

### HTTP headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

### Authentication scheme

api_key





# **get_inventory_in_object**
> get_inventory_in_object()

Fake endpoint to test arbitrary object return by 'Get inventory'

Returns an arbitrary object which is actually a map of status codes to quantities

### Sample 
```perl
my $api = WWW::SwaggerClient::StoreApi->new();

eval { 
    my $result = $api->get_inventory_in_object();
};
if ($@) {
    warn "Exception when calling get_inventory_in_object: $@\n";
}
```

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------

### Return type

[**object**](object.md)

### HTTP headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

### Authentication scheme

api_key





# **place_order**
> place_order(body => $body)

Place an order for a pet



### Sample 
```perl
my $api = WWW::SwaggerClient::StoreApi->new();
my $body = WWW::SwaggerClient::Object::Order->new(); # [Order] order placed for purchasing the pet

eval { 
    my $result = $api->place_order(body => $body);
};
if ($@) {
    warn "Exception when calling place_order: $@\n";
}
```

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Order**](docs/.md)| order placed for purchasing the pet | [optional] 

### Return type

[**Order**](Order.md)

### HTTP headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

### Authentication scheme

test_api_client_idtest_api_client_secret





# **get_order_by_id**
> get_order_by_id(order_id => $order_id)

Find purchase order by ID

For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions

### Sample 
```perl
my $api = WWW::SwaggerClient::StoreApi->new();
my $order_id = 'order_id_example'; # [string] ID of pet that needs to be fetched

eval { 
    my $result = $api->get_order_by_id(order_id => $order_id);
};
if ($@) {
    warn "Exception when calling get_order_by_id: $@\n";
}
```

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **order_id** | [**string**](docs/.md)| ID of pet that needs to be fetched | 

### Return type

[**Order**](Order.md)

### HTTP headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

### Authentication scheme

test_api_key_headertest_api_key_query





# **delete_order**
> delete_order(order_id => $order_id)

Delete purchase order by ID

For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors

### Sample 
```perl
my $api = WWW::SwaggerClient::StoreApi->new();
my $order_id = 'order_id_example'; # [string] ID of the order that needs to be deleted

eval { 
    my $result = $api->delete_order(order_id => $order_id);
};
if ($@) {
    warn "Exception when calling delete_order: $@\n";
}
```

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **order_id** | [**string**](docs/.md)| ID of the order that needs to be deleted | 

### Return type

void (empty response body)

### HTTP headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

### Authentication scheme

No authentiation required





