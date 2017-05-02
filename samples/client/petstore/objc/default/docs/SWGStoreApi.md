# SWGStoreApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**deleteOrder**](SWGStoreApi.md#deleteorder) | **DELETE** /store/order/{orderId} | Delete purchase order by ID
[**getInventory**](SWGStoreApi.md#getinventory) | **GET** /store/inventory | Returns pet inventories by status
[**getOrderById**](SWGStoreApi.md#getorderbyid) | **GET** /store/order/{orderId} | Find purchase order by ID
[**placeOrder**](SWGStoreApi.md#placeorder) | **POST** /store/order | Place an order for a pet


# **deleteOrder**
```objc
-(NSURLSessionTask*) deleteOrderWithOrderId: (NSString*) orderId
        completionHandler: (void (^)(NSError* error)) handler;
```

Delete purchase order by ID

For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors

### Example 
```objc

NSString* orderId = @"orderId_example"; // ID of the order that needs to be deleted

SWGStoreApi*apiInstance = [[SWGStoreApi alloc] init];

// Delete purchase order by ID
[apiInstance deleteOrderWithOrderId:orderId
          completionHandler: ^(NSError* error) {
                        if (error) {
                            NSLog(@"Error calling SWGStoreApi->deleteOrder: %@", error);
                        }
                    }];
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **orderId** | **NSString***| ID of the order that needs to be deleted | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **getInventory**
```objc
-(NSURLSessionTask*) getInventoryWithCompletionHandler: 
        (void (^)(NSDictionary<NSString*, NSNumber*>* output, NSError* error)) handler;
```

Returns pet inventories by status

Returns a map of status codes to quantities

### Example 
```objc
SWGDefaultConfiguration *apiConfig = [SWGDefaultConfiguration sharedConfig];

// Configure API key authorization: (authentication scheme: api_key)
[apiConfig setApiKey:@"YOUR_API_KEY" forApiKeyIdentifier:@"api_key"];
// Uncomment below to setup prefix (e.g. Bearer) for API key, if needed
//[apiConfig setApiKeyPrefix:@"Bearer" forApiKeyIdentifier:@"api_key"];



SWGStoreApi*apiInstance = [[SWGStoreApi alloc] init];

// Returns pet inventories by status
[apiInstance getInventoryWithCompletionHandler: 
          ^(NSDictionary<NSString*, NSNumber*>* output, NSError* error) {
                        if (output) {
                            NSLog(@"%@", output);
                        }
                        if (error) {
                            NSLog(@"Error calling SWGStoreApi->getInventory: %@", error);
                        }
                    }];
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**NSDictionary<NSString*, NSNumber*>***](NSDictionary.md)

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **getOrderById**
```objc
-(NSURLSessionTask*) getOrderByIdWithOrderId: (NSNumber*) orderId
        completionHandler: (void (^)(SWGOrder* output, NSError* error)) handler;
```

Find purchase order by ID

For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions

### Example 
```objc

NSNumber* orderId = @789; // ID of pet that needs to be fetched

SWGStoreApi*apiInstance = [[SWGStoreApi alloc] init];

// Find purchase order by ID
[apiInstance getOrderByIdWithOrderId:orderId
          completionHandler: ^(SWGOrder* output, NSError* error) {
                        if (output) {
                            NSLog(@"%@", output);
                        }
                        if (error) {
                            NSLog(@"Error calling SWGStoreApi->getOrderById: %@", error);
                        }
                    }];
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **orderId** | **NSNumber***| ID of pet that needs to be fetched | 

### Return type

[**SWGOrder***](SWGOrder.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **placeOrder**
```objc
-(NSURLSessionTask*) placeOrderWithBody: (SWGOrder*) body
        completionHandler: (void (^)(SWGOrder* output, NSError* error)) handler;
```

Place an order for a pet



### Example 
```objc

SWGOrder* body = [[SWGOrder alloc] init]; // order placed for purchasing the pet

SWGStoreApi*apiInstance = [[SWGStoreApi alloc] init];

// Place an order for a pet
[apiInstance placeOrderWithBody:body
          completionHandler: ^(SWGOrder* output, NSError* error) {
                        if (output) {
                            NSLog(@"%@", output);
                        }
                        if (error) {
                            NSLog(@"Error calling SWGStoreApi->placeOrder: %@", error);
                        }
                    }];
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**SWGOrder***](SWGOrder*.md)| order placed for purchasing the pet | 

### Return type

[**SWGOrder***](SWGOrder.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

