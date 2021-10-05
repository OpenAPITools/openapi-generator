# FakeAPI

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**createXmlItem**](FakeAPI.md#createxmlitem) | **POST** /fake/create_xml_item | creates an XmlItem
[**fakeOuterBooleanSerialize**](FakeAPI.md#fakeouterbooleanserialize) | **POST** /fake/outer/boolean | 
[**fakeOuterCompositeSerialize**](FakeAPI.md#fakeoutercompositeserialize) | **POST** /fake/outer/composite | 
[**fakeOuterNumberSerialize**](FakeAPI.md#fakeouternumberserialize) | **POST** /fake/outer/number | 
[**fakeOuterStringSerialize**](FakeAPI.md#fakeouterstringserialize) | **POST** /fake/outer/string | 
[**testBodyWithFileSchema**](FakeAPI.md#testbodywithfileschema) | **PUT** /fake/body-with-file-schema | 
[**testBodyWithQueryParams**](FakeAPI.md#testbodywithqueryparams) | **PUT** /fake/body-with-query-params | 
[**testClientModel**](FakeAPI.md#testclientmodel) | **PATCH** /fake | To test \&quot;client\&quot; model
[**testEndpointParameters**](FakeAPI.md#testendpointparameters) | **POST** /fake | Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
[**testEnumParameters**](FakeAPI.md#testenumparameters) | **GET** /fake | To test enum parameters
[**testGroupParameters**](FakeAPI.md#testgroupparameters) | **DELETE** /fake | Fake endpoint to test group parameters (optional)
[**testInlineAdditionalProperties**](FakeAPI.md#testinlineadditionalproperties) | **POST** /fake/inline-additionalProperties | test inline additionalProperties
[**testJsonFormData**](FakeAPI.md#testjsonformdata) | **GET** /fake/jsonFormData | test json serialization of form data
[**testQueryParameterCollectionFormat**](FakeAPI.md#testqueryparametercollectionformat) | **PUT** /fake/test-query-parameters | 


# **createXmlItem**
```swift
    open class func createXmlItem(xmlItem: XmlItem, headers: HTTPHeaders = PetstoreClientAPI.customHeaders, beforeSend: (inout ClientRequest) throws -> () = { _ in }) -> EventLoopFuture<CreateXmlItem>
```

creates an XmlItem

this route creates an XmlItem

### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient

let xmlItem = XmlItem(attributeString: "attributeString_example", attributeNumber: 123, attributeInteger: 123, attributeBoolean: true, wrappedArray: [123], nameString: "nameString_example", nameNumber: 123, nameInteger: 123, nameBoolean: true, nameArray: [123], nameWrappedArray: [123], prefixString: "prefixString_example", prefixNumber: 123, prefixInteger: 123, prefixBoolean: true, prefixArray: [123], prefixWrappedArray: [123], namespaceString: "namespaceString_example", namespaceNumber: 123, namespaceInteger: 123, namespaceBoolean: true, namespaceArray: [123], namespaceWrappedArray: [123], prefixNsString: "prefixNsString_example", prefixNsNumber: 123, prefixNsInteger: 123, prefixNsBoolean: true, prefixNsArray: [123], prefixNsWrappedArray: [123]) // XmlItem | XmlItem Body

// creates an XmlItem
FakeAPI.createXmlItem(xmlItem: xmlItem).whenComplete { result in
    switch result {
    case .failure(let error):
    // process error
    case .success(let response):
        switch response {
        // process decoded response value or raw ClientResponse
        case .http200(let value, let raw):
        case .http0(let value, let raw):
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **xmlItem** | [**XmlItem**](XmlItem.md) | XmlItem Body | 

### Return type

#### CreateXmlItem

```swift
public enum CreateXmlItem {
    case http200(value: Void?, raw: ClientResponse)
    case http0(value: Void?, raw: ClientResponse)
}
```

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/xml, application/xml; charset=utf-8, application/xml; charset=utf-16, text/xml, text/xml; charset=utf-8, text/xml; charset=utf-16
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **fakeOuterBooleanSerialize**
```swift
    open class func fakeOuterBooleanSerialize(body: Bool? = nil, headers: HTTPHeaders = PetstoreClientAPI.customHeaders, beforeSend: (inout ClientRequest) throws -> () = { _ in }) -> EventLoopFuture<FakeOuterBooleanSerialize>
```



Test serialization of outer boolean types

### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient

let body = true // Bool | Input boolean as post body (optional)

FakeAPI.fakeOuterBooleanSerialize(body: body).whenComplete { result in
    switch result {
    case .failure(let error):
    // process error
    case .success(let response):
        switch response {
        // process decoded response value or raw ClientResponse
        case .http200(let value, let raw):
        case .http0(let value, let raw):
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **Bool** | Input boolean as post body | [optional] 

### Return type

#### FakeOuterBooleanSerialize

```swift
public enum FakeOuterBooleanSerialize {
    case http200(value: Bool?, raw: ClientResponse)
    case http0(value: Bool?, raw: ClientResponse)
}
```

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **fakeOuterCompositeSerialize**
```swift
    open class func fakeOuterCompositeSerialize(body: OuterComposite? = nil, headers: HTTPHeaders = PetstoreClientAPI.customHeaders, beforeSend: (inout ClientRequest) throws -> () = { _ in }) -> EventLoopFuture<FakeOuterCompositeSerialize>
```



Test serialization of object with outer number type

### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient

let body = OuterComposite(myNumber: 123, myString: "myString_example", myBoolean: false) // OuterComposite | Input composite as post body (optional)

FakeAPI.fakeOuterCompositeSerialize(body: body).whenComplete { result in
    switch result {
    case .failure(let error):
    // process error
    case .success(let response):
        switch response {
        // process decoded response value or raw ClientResponse
        case .http200(let value, let raw):
        case .http0(let value, let raw):
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**OuterComposite**](OuterComposite.md) | Input composite as post body | [optional] 

### Return type

#### FakeOuterCompositeSerialize

```swift
public enum FakeOuterCompositeSerialize {
    case http200(value: OuterComposite?, raw: ClientResponse)
    case http0(value: OuterComposite?, raw: ClientResponse)
}
```

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **fakeOuterNumberSerialize**
```swift
    open class func fakeOuterNumberSerialize(body: Double? = nil, headers: HTTPHeaders = PetstoreClientAPI.customHeaders, beforeSend: (inout ClientRequest) throws -> () = { _ in }) -> EventLoopFuture<FakeOuterNumberSerialize>
```



Test serialization of outer number types

### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient

let body = 987 // Double | Input number as post body (optional)

FakeAPI.fakeOuterNumberSerialize(body: body).whenComplete { result in
    switch result {
    case .failure(let error):
    // process error
    case .success(let response):
        switch response {
        // process decoded response value or raw ClientResponse
        case .http200(let value, let raw):
        case .http0(let value, let raw):
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **Double** | Input number as post body | [optional] 

### Return type

#### FakeOuterNumberSerialize

```swift
public enum FakeOuterNumberSerialize {
    case http200(value: Double?, raw: ClientResponse)
    case http0(value: Double?, raw: ClientResponse)
}
```

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **fakeOuterStringSerialize**
```swift
    open class func fakeOuterStringSerialize(body: String? = nil, headers: HTTPHeaders = PetstoreClientAPI.customHeaders, beforeSend: (inout ClientRequest) throws -> () = { _ in }) -> EventLoopFuture<FakeOuterStringSerialize>
```



Test serialization of outer string types

### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient

let body = "body_example" // String | Input string as post body (optional)

FakeAPI.fakeOuterStringSerialize(body: body).whenComplete { result in
    switch result {
    case .failure(let error):
    // process error
    case .success(let response):
        switch response {
        // process decoded response value or raw ClientResponse
        case .http200(let value, let raw):
        case .http0(let value, let raw):
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **String** | Input string as post body | [optional] 

### Return type

#### FakeOuterStringSerialize

```swift
public enum FakeOuterStringSerialize {
    case http200(value: String?, raw: ClientResponse)
    case http0(value: String?, raw: ClientResponse)
}
```

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testBodyWithFileSchema**
```swift
    open class func testBodyWithFileSchema(body: FileSchemaTestClass, headers: HTTPHeaders = PetstoreClientAPI.customHeaders, beforeSend: (inout ClientRequest) throws -> () = { _ in }) -> EventLoopFuture<TestBodyWithFileSchema>
```



For this test, the body for this request much reference a schema named `File`.

### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient

let body = FileSchemaTestClass(file: File(sourceURI: "sourceURI_example"), files: [nil]) // FileSchemaTestClass | 

FakeAPI.testBodyWithFileSchema(body: body).whenComplete { result in
    switch result {
    case .failure(let error):
    // process error
    case .success(let response):
        switch response {
        // process decoded response value or raw ClientResponse
        case .http200(let value, let raw):
        case .http0(let value, let raw):
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**FileSchemaTestClass**](FileSchemaTestClass.md) |  | 

### Return type

#### TestBodyWithFileSchema

```swift
public enum TestBodyWithFileSchema {
    case http200(value: Void?, raw: ClientResponse)
    case http0(value: Void?, raw: ClientResponse)
}
```

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testBodyWithQueryParams**
```swift
    open class func testBodyWithQueryParams(query: String, body: User, headers: HTTPHeaders = PetstoreClientAPI.customHeaders, beforeSend: (inout ClientRequest) throws -> () = { _ in }) -> EventLoopFuture<TestBodyWithQueryParams>
```



### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient

let query = "query_example" // String | 
let body = User(id: 123, username: "username_example", firstName: "firstName_example", lastName: "lastName_example", email: "email_example", password: "password_example", phone: "phone_example", userStatus: 123) // User | 

FakeAPI.testBodyWithQueryParams(query: query, body: body).whenComplete { result in
    switch result {
    case .failure(let error):
    // process error
    case .success(let response):
        switch response {
        // process decoded response value or raw ClientResponse
        case .http200(let value, let raw):
        case .http0(let value, let raw):
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **query** | **String** |  | 
 **body** | [**User**](User.md) |  | 

### Return type

#### TestBodyWithQueryParams

```swift
public enum TestBodyWithQueryParams {
    case http200(value: Void?, raw: ClientResponse)
    case http0(value: Void?, raw: ClientResponse)
}
```

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testClientModel**
```swift
    open class func testClientModel(body: Client, headers: HTTPHeaders = PetstoreClientAPI.customHeaders, beforeSend: (inout ClientRequest) throws -> () = { _ in }) -> EventLoopFuture<TestClientModel>
```

To test \"client\" model

To test \"client\" model

### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient

let body = Client(client: "client_example") // Client | client model

// To test \"client\" model
FakeAPI.testClientModel(body: body).whenComplete { result in
    switch result {
    case .failure(let error):
    // process error
    case .success(let response):
        switch response {
        // process decoded response value or raw ClientResponse
        case .http200(let value, let raw):
        case .http0(let value, let raw):
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Client**](Client.md) | client model | 

### Return type

#### TestClientModel

```swift
public enum TestClientModel {
    case http200(value: Client?, raw: ClientResponse)
    case http0(value: Client?, raw: ClientResponse)
}
```

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testEndpointParameters**
```swift
    open class func testEndpointParameters(number: Double, double: Double, patternWithoutDelimiter: String, byte: Data, integer: Int? = nil, int32: Int? = nil, int64: Int64? = nil, float: Float? = nil, string: String? = nil, binary: Data? = nil, date: Date? = nil, dateTime: Date? = nil, password: String? = nil, callback: String? = nil, headers: HTTPHeaders = PetstoreClientAPI.customHeaders, beforeSend: (inout ClientRequest) throws -> () = { _ in }) -> EventLoopFuture<TestEndpointParameters>
```

Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트

Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트

### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient

let number = 987 // Double | None
let double = 987 // Double | None
let patternWithoutDelimiter = "patternWithoutDelimiter_example" // String | None
let byte = Data([9, 8, 7]) // Data | None
let integer = 987 // Int | None (optional)
let int32 = 987 // Int | None (optional)
let int64 = 987 // Int64 | None (optional)
let float = 987 // Float | None (optional)
let string = "string_example" // String | None (optional)
let binary = Data([9, 8, 7]) // Data | None (optional)
let date = Date() // Date | None (optional)
let dateTime = Date() // Date | None (optional)
let password = "password_example" // String | None (optional)
let callback = "callback_example" // String | None (optional)

// Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
FakeAPI.testEndpointParameters(number: number, double: double, patternWithoutDelimiter: patternWithoutDelimiter, byte: byte, integer: integer, int32: int32, int64: int64, float: float, string: string, binary: binary, date: date, dateTime: dateTime, password: password, callback: callback).whenComplete { result in
    switch result {
    case .failure(let error):
    // process error
    case .success(let response):
        switch response {
        // process decoded response value or raw ClientResponse
        case .http400(let value, let raw):
        case .http404(let value, let raw):
        case .http0(let value, let raw):
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **number** | **Double** | None | 
 **double** | **Double** | None | 
 **patternWithoutDelimiter** | **String** | None | 
 **byte** | **Data** | None | 
 **integer** | **Int** | None | [optional] 
 **int32** | **Int** | None | [optional] 
 **int64** | **Int64** | None | [optional] 
 **float** | **Float** | None | [optional] 
 **string** | **String** | None | [optional] 
 **binary** | **Data** | None | [optional] 
 **date** | **Date** | None | [optional] 
 **dateTime** | **Date** | None | [optional] 
 **password** | **String** | None | [optional] 
 **callback** | **String** | None | [optional] 

### Return type

#### TestEndpointParameters

```swift
public enum TestEndpointParameters {
    case http400(value: Void?, raw: ClientResponse)
    case http404(value: Void?, raw: ClientResponse)
    case http0(value: Void?, raw: ClientResponse)
}
```

### Authorization

[http_basic_test](../README.md#http_basic_test)

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testEnumParameters**
```swift
    open class func testEnumParameters(enumHeaderStringArray: [String]? = nil, enumHeaderString: EnumHeaderString_testEnumParameters? = nil, enumQueryStringArray: [String]? = nil, enumQueryString: EnumQueryString_testEnumParameters? = nil, enumQueryInteger: EnumQueryInteger_testEnumParameters? = nil, enumQueryDouble: EnumQueryDouble_testEnumParameters? = nil, enumFormStringArray: [String]? = nil, enumFormString: EnumFormString_testEnumParameters? = nil, headers: HTTPHeaders = PetstoreClientAPI.customHeaders, beforeSend: (inout ClientRequest) throws -> () = { _ in }) -> EventLoopFuture<TestEnumParameters>
```

To test enum parameters

To test enum parameters

### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient

let enumHeaderStringArray = ["enumHeaderStringArray_example"] // [String] | Header parameter enum test (string array) (optional)
let enumHeaderString = "enumHeaderString_example" // String | Header parameter enum test (string) (optional) (default to .efg)
let enumQueryStringArray = ["enumQueryStringArray_example"] // [String] | Query parameter enum test (string array) (optional)
let enumQueryString = "enumQueryString_example" // String | Query parameter enum test (string) (optional) (default to .efg)
let enumQueryInteger = 987 // Int | Query parameter enum test (double) (optional)
let enumQueryDouble = 987 // Double | Query parameter enum test (double) (optional)
let enumFormStringArray = ["inner_example"] // [String] | Form parameter enum test (string array) (optional) (default to .dollar)
let enumFormString = "enumFormString_example" // String | Form parameter enum test (string) (optional) (default to .efg)

// To test enum parameters
FakeAPI.testEnumParameters(enumHeaderStringArray: enumHeaderStringArray, enumHeaderString: enumHeaderString, enumQueryStringArray: enumQueryStringArray, enumQueryString: enumQueryString, enumQueryInteger: enumQueryInteger, enumQueryDouble: enumQueryDouble, enumFormStringArray: enumFormStringArray, enumFormString: enumFormString).whenComplete { result in
    switch result {
    case .failure(let error):
    // process error
    case .success(let response):
        switch response {
        // process decoded response value or raw ClientResponse
        case .http400(let value, let raw):
        case .http404(let value, let raw):
        case .http0(let value, let raw):
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **enumHeaderStringArray** | [**[String]**](String.md) | Header parameter enum test (string array) | [optional] 
 **enumHeaderString** | **String** | Header parameter enum test (string) | [optional] [default to .efg]
 **enumQueryStringArray** | [**[String]**](String.md) | Query parameter enum test (string array) | [optional] 
 **enumQueryString** | **String** | Query parameter enum test (string) | [optional] [default to .efg]
 **enumQueryInteger** | **Int** | Query parameter enum test (double) | [optional] 
 **enumQueryDouble** | **Double** | Query parameter enum test (double) | [optional] 
 **enumFormStringArray** | [**[String]**](String.md) | Form parameter enum test (string array) | [optional] [default to .dollar]
 **enumFormString** | **String** | Form parameter enum test (string) | [optional] [default to .efg]

### Return type

#### TestEnumParameters

```swift
public enum TestEnumParameters {
    case http400(value: Void?, raw: ClientResponse)
    case http404(value: Void?, raw: ClientResponse)
    case http0(value: Void?, raw: ClientResponse)
}
```

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testGroupParameters**
```swift
    open class func testGroupParameters(requiredStringGroup: Int, requiredBooleanGroup: Bool, requiredInt64Group: Int64, stringGroup: Int? = nil, booleanGroup: Bool? = nil, int64Group: Int64? = nil, headers: HTTPHeaders = PetstoreClientAPI.customHeaders, beforeSend: (inout ClientRequest) throws -> () = { _ in }) -> EventLoopFuture<TestGroupParameters>
```

Fake endpoint to test group parameters (optional)

Fake endpoint to test group parameters (optional)

### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient

let requiredStringGroup = 987 // Int | Required String in group parameters
let requiredBooleanGroup = true // Bool | Required Boolean in group parameters
let requiredInt64Group = 987 // Int64 | Required Integer in group parameters
let stringGroup = 987 // Int | String in group parameters (optional)
let booleanGroup = true // Bool | Boolean in group parameters (optional)
let int64Group = 987 // Int64 | Integer in group parameters (optional)

// Fake endpoint to test group parameters (optional)
FakeAPI.testGroupParameters(requiredStringGroup: requiredStringGroup, requiredBooleanGroup: requiredBooleanGroup, requiredInt64Group: requiredInt64Group, stringGroup: stringGroup, booleanGroup: booleanGroup, int64Group: int64Group).whenComplete { result in
    switch result {
    case .failure(let error):
    // process error
    case .success(let response):
        switch response {
        // process decoded response value or raw ClientResponse
        case .http400(let value, let raw):
        case .http0(let value, let raw):
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **requiredStringGroup** | **Int** | Required String in group parameters | 
 **requiredBooleanGroup** | **Bool** | Required Boolean in group parameters | 
 **requiredInt64Group** | **Int64** | Required Integer in group parameters | 
 **stringGroup** | **Int** | String in group parameters | [optional] 
 **booleanGroup** | **Bool** | Boolean in group parameters | [optional] 
 **int64Group** | **Int64** | Integer in group parameters | [optional] 

### Return type

#### TestGroupParameters

```swift
public enum TestGroupParameters {
    case http400(value: Void?, raw: ClientResponse)
    case http0(value: Void?, raw: ClientResponse)
}
```

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testInlineAdditionalProperties**
```swift
    open class func testInlineAdditionalProperties(param: [String: String], headers: HTTPHeaders = PetstoreClientAPI.customHeaders, beforeSend: (inout ClientRequest) throws -> () = { _ in }) -> EventLoopFuture<TestInlineAdditionalProperties>
```

test inline additionalProperties

### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient

let param = "TODO" // [String: String] | request body

// test inline additionalProperties
FakeAPI.testInlineAdditionalProperties(param: param).whenComplete { result in
    switch result {
    case .failure(let error):
    // process error
    case .success(let response):
        switch response {
        // process decoded response value or raw ClientResponse
        case .http200(let value, let raw):
        case .http0(let value, let raw):
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **param** | [**[String: String]**](String.md) | request body | 

### Return type

#### TestInlineAdditionalProperties

```swift
public enum TestInlineAdditionalProperties {
    case http200(value: Void?, raw: ClientResponse)
    case http0(value: Void?, raw: ClientResponse)
}
```

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testJsonFormData**
```swift
    open class func testJsonFormData(param: String, param2: String, headers: HTTPHeaders = PetstoreClientAPI.customHeaders, beforeSend: (inout ClientRequest) throws -> () = { _ in }) -> EventLoopFuture<TestJsonFormData>
```

test json serialization of form data

### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient

let param = "param_example" // String | field1
let param2 = "param2_example" // String | field2

// test json serialization of form data
FakeAPI.testJsonFormData(param: param, param2: param2).whenComplete { result in
    switch result {
    case .failure(let error):
    // process error
    case .success(let response):
        switch response {
        // process decoded response value or raw ClientResponse
        case .http200(let value, let raw):
        case .http0(let value, let raw):
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **param** | **String** | field1 | 
 **param2** | **String** | field2 | 

### Return type

#### TestJsonFormData

```swift
public enum TestJsonFormData {
    case http200(value: Void?, raw: ClientResponse)
    case http0(value: Void?, raw: ClientResponse)
}
```

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **testQueryParameterCollectionFormat**
```swift
    open class func testQueryParameterCollectionFormat(pipe: [String], ioutil: [String], http: [String], url: [String], context: [String], headers: HTTPHeaders = PetstoreClientAPI.customHeaders, beforeSend: (inout ClientRequest) throws -> () = { _ in }) -> EventLoopFuture<TestQueryParameterCollectionFormat>
```



To test the collection format in query parameters

### Example
```swift
// The following code samples are still beta. For any issue, please report via http://github.com/OpenAPITools/openapi-generator/issues/new
import PetstoreClient

let pipe = ["inner_example"] // [String] | 
let ioutil = ["inner_example"] // [String] | 
let http = ["inner_example"] // [String] | 
let url = ["inner_example"] // [String] | 
let context = ["inner_example"] // [String] | 

FakeAPI.testQueryParameterCollectionFormat(pipe: pipe, ioutil: ioutil, http: http, url: url, context: context).whenComplete { result in
    switch result {
    case .failure(let error):
    // process error
    case .success(let response):
        switch response {
        // process decoded response value or raw ClientResponse
        case .http200(let value, let raw):
        case .http0(let value, let raw):
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pipe** | [**[String]**](String.md) |  | 
 **ioutil** | [**[String]**](String.md) |  | 
 **http** | [**[String]**](String.md) |  | 
 **url** | [**[String]**](String.md) |  | 
 **context** | [**[String]**](String.md) |  | 

### Return type

#### TestQueryParameterCollectionFormat

```swift
public enum TestQueryParameterCollectionFormat {
    case http200(value: Void?, raw: ClientResponse)
    case http0(value: Void?, raw: ClientResponse)
}
```

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

