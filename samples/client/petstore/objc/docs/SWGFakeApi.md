# SWGFakeApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**testEndpointParameters**](SWGFakeApi.md#testendpointparameters) | **POST** /fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 


# **testEndpointParameters**
```objc
-(NSNumber*) testEndpointParametersWithNumber: (NSNumber*) number
    _double: (NSNumber*) _double
    string: (NSString*) string
    byte: (NSData*) byte
    integer: (NSNumber*) integer
    int32: (NSNumber*) int32
    int64: (NSNumber*) int64
    _float: (NSNumber*) _float
    binary: (NSData*) binary
    date: (NSDate*) date
    dateTime: (NSDate*) dateTime
    password: (NSString*) password
        completionHandler: (void (^)(NSError* error)) handler;
```

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

### Example 
```objc

NSNumber* number = @3.4; // None
NSNumber* _double = @1.2; // None
NSString* string = @"string_example"; // None
NSData* byte = [[NSData alloc] init]; // None
NSNumber* integer = @56; // None (optional)
NSNumber* int32 = @56; // None (optional)
NSNumber* int64 = @789; // None (optional)
NSNumber* _float = @3.4; // None (optional)
NSData* binary = [[NSData alloc] init]; // None (optional)
NSDate* date = @"2013-10-20"; // None (optional)
NSDate* dateTime = @"2013-10-20T19:20:30+01:00"; // None (optional)
NSString* password = @"password_example"; // None (optional)

SWGFakeApi*apiInstance = [[SWGFakeApi alloc] init];

// Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
[apiInstance testEndpointParametersWithNumber:number
              _double:_double
              string:string
              byte:byte
              integer:integer
              int32:int32
              int64:int64
              _float:_float
              binary:binary
              date:date
              dateTime:dateTime
              password:password
          completionHandler: ^(NSError* error) {
                        if (error) {
                            NSLog(@"Error calling SWGFakeApi->testEndpointParameters: %@", error);
                        }
                    }];
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **number** | **NSNumber***| None | 
 **_double** | **NSNumber***| None | 
 **string** | **NSString***| None | 
 **byte** | **NSData***| None | 
 **integer** | **NSNumber***| None | [optional] 
 **int32** | **NSNumber***| None | [optional] 
 **int64** | **NSNumber***| None | [optional] 
 **_float** | **NSNumber***| None | [optional] 
 **binary** | **NSData***| None | [optional] 
 **date** | **NSDate***| None | [optional] 
 **dateTime** | **NSDate***| None | [optional] 
 **password** | **NSString***| None | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/xml; charset=utf-8, application/json; charset=utf-8
 - **Accept**: application/xml; charset=utf-8, application/json; charset=utf-8

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

