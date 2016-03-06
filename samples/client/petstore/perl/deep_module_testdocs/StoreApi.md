# Something::Deep::StoreApi

- [find_orders_by_status](#find_orders_by_status): Finds orders by status
- [get_inventory](#get_inventory): Returns pet inventories by status
- [get_inventory_in_object](#get_inventory_in_object): Fake endpoint to test arbitrary object return by &#39;Get inventory&#39;
- [place_order](#place_order): Place an order for a pet
- [get_order_by_id](#get_order_by_id): Find purchase order by ID
- [delete_order](#delete_order): Delete purchase order by ID

## **find_orders_by_status**

Finds orders by status

A single status value can be provided as a string

### Sample 
```perl
my $api = Something::Deep::StoreApi->new();
my $status = 'status_example'; # [string] Status value that needs to be considered for query

eval { 
    my $result = $api->find_orders_by_status(status => $status);
};
if ($@) {
    warn "Exception when calling find_orders_by_status: $@\n";
}
```

### Parameters
Required | Name | Type | Description 
------------ | ------------- | ------------- | -------------
 No | status | string | Status value that needs to be considered for query

### Return type

ARRAY[Order]

### HTTP headers

Content-Type: Not defined
Accept: application/json, application/xml

### Authentication scheme

test_api_client_id test_api_client_secret 


## **get_inventory**

Returns pet inventories by status

Returns a map of status codes to quantities

### Sample 
```perl
my $api = Something::Deep::StoreApi->new();

eval { 
    my $result = $api->get_inventory();
};
if ($@) {
    warn "Exception when calling get_inventory: $@\n";
}
```

### Parameters
Required | Name | Type | Description 
------------ | ------------- | ------------- | -------------

### Return type

HASH[string,int]

### HTTP headers

Content-Type: Not defined
Accept: application/json, application/xml

### Authentication scheme

api_key 


## **get_inventory_in_object**

Fake endpoint to test arbitrary object return by 'Get inventory'

Returns an arbitrary object which is actually a map of status codes to quantities

### Sample 
```perl
my $api = Something::Deep::StoreApi->new();

eval { 
    my $result = $api->get_inventory_in_object();
};
if ($@) {
    warn "Exception when calling get_inventory_in_object: $@\n";
}
```

### Parameters
Required | Name | Type | Description 
------------ | ------------- | ------------- | -------------

### Return type

object

### HTTP headers

Content-Type: Not defined
Accept: application/json, application/xml

### Authentication scheme

api_key 


## **place_order**

Place an order for a pet



### Sample 
```perl
my $api = Something::Deep::StoreApi->new();
my $body = new Something::Deep::Object::Order->new(); # [Order] order placed for purchasing the pet

eval { 
    my $result = $api->place_order(body => $body);
};
if ($@) {
    warn "Exception when calling place_order: $@\n";
}
```

### Parameters
Required | Name | Type | Description 
------------ | ------------- | ------------- | -------------
 No | body | Order | order placed for purchasing the pet

### Return type

Order

### HTTP headers

Content-Type: Not defined
Accept: application/json, application/xml

### Authentication scheme

test_api_client_id test_api_client_secret 


## **get_order_by_id**

Find purchase order by ID

For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions

### Sample 
```perl
my $api = Something::Deep::StoreApi->new();
my $order_id = 'order_id_example'; # [string] ID of pet that needs to be fetched

eval { 
    my $result = $api->get_order_by_id(order_id => $order_id);
};
if ($@) {
    warn "Exception when calling get_order_by_id: $@\n";
}
```

### Parameters
Required | Name | Type | Description 
------------ | ------------- | ------------- | -------------
 Yes | order_id | string | ID of pet that needs to be fetched

### Return type

Order

### HTTP headers

Content-Type: Not defined
Accept: application/json, application/xml

### Authentication scheme

test_api_key_header test_api_key_query 


## **delete_order**

Delete purchase order by ID

For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors

### Sample 
```perl
my $api = Something::Deep::StoreApi->new();
my $order_id = 'order_id_example'; # [string] ID of the order that needs to be deleted

eval { 
    my $result = $api->delete_order(order_id => $order_id);
};
if ($@) {
    warn "Exception when calling delete_order: $@\n";
}
```

### Parameters
Required | Name | Type | Description 
------------ | ------------- | ------------- | -------------
 Yes | order_id | string | ID of the order that needs to be deleted

### Return type

void (empty response body)

### HTTP headers

Content-Type: Not defined
Accept: application/json, application/xml

### Authentication scheme




1;
