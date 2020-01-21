# OpenApiPetstore.FakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**createXmlItem**](FakeApi.md#createXmlItem) | **POST** /fake/create_xml_item | creates an XmlItem
[**fakeOuterBooleanSerialize**](FakeApi.md#fakeOuterBooleanSerialize) | **POST** /fake/outer/boolean | 
[**fakeOuterCompositeSerialize**](FakeApi.md#fakeOuterCompositeSerialize) | **POST** /fake/outer/composite | 
[**fakeOuterNumberSerialize**](FakeApi.md#fakeOuterNumberSerialize) | **POST** /fake/outer/number | 
[**fakeOuterStringSerialize**](FakeApi.md#fakeOuterStringSerialize) | **POST** /fake/outer/string | 
[**testBodyWithFileSchema**](FakeApi.md#testBodyWithFileSchema) | **PUT** /fake/body-with-file-schema | 
[**testBodyWithQueryParams**](FakeApi.md#testBodyWithQueryParams) | **PUT** /fake/body-with-query-params | 
[**testClientModel**](FakeApi.md#testClientModel) | **PATCH** /fake | To test \&quot;client\&quot; model
[**testEndpointParameters**](FakeApi.md#testEndpointParameters) | **POST** /fake | Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
[**testEnumParameters**](FakeApi.md#testEnumParameters) | **GET** /fake | To test enum parameters
[**testGroupParameters**](FakeApi.md#testGroupParameters) | **DELETE** /fake | Fake endpoint to test group parameters (optional)
[**testInlineAdditionalProperties**](FakeApi.md#testInlineAdditionalProperties) | **POST** /fake/inline-additionalProperties | test inline additionalProperties
[**testJsonFormData**](FakeApi.md#testJsonFormData) | **GET** /fake/jsonFormData | test json serialization of form data
[**testQueryParameterCollectionFormat**](FakeApi.md#testQueryParameterCollectionFormat) | **PUT** /fake/test-query-paramters | 



## createXmlItem

> createXmlItem(xmlItem)

creates an XmlItem

this route creates an XmlItem

### Example

```javascript
var OpenApiPetstore = require('open_api_petstore');

var apiInstance = new OpenApiPetstore.FakeApi();
var xmlItem = new OpenApiPetstore.XmlItem(); // XmlItem | XmlItem Body
var callback = function(error, data, response) {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully.');
  }
};
apiInstance.createXmlItem(xmlItem, callback);
```

### Parameters



Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **xmlItem** | [**XmlItem**](XmlItem.md)| XmlItem Body | 

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/xml, application/xml; charset=utf-8, application/xml; charset=utf-16, text/xml, text/xml; charset=utf-8, text/xml; charset=utf-16
- **Accept**: Not defined


## fakeOuterBooleanSerialize

> Boolean fakeOuterBooleanSerialize(opts)



Test serialization of outer boolean types

### Example

```javascript
var OpenApiPetstore = require('open_api_petstore');

var apiInstance = new OpenApiPetstore.FakeApi();
var opts = {
  'body': true // Boolean | Input boolean as post body
};
var callback = function(error, data, response) {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully. Returned data: ' + data);
  }
};
apiInstance.fakeOuterBooleanSerialize(opts, callback);
```

### Parameters



Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **Boolean**| Input boolean as post body | [optional] 

### Return type

**Boolean**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: */*


## fakeOuterCompositeSerialize

> OuterComposite fakeOuterCompositeSerialize(opts)



Test serialization of object with outer number type

### Example

```javascript
var OpenApiPetstore = require('open_api_petstore');

var apiInstance = new OpenApiPetstore.FakeApi();
var opts = {
  'body': new OpenApiPetstore.OuterComposite() // OuterComposite | Input composite as post body
};
var callback = function(error, data, response) {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully. Returned data: ' + data);
  }
};
apiInstance.fakeOuterCompositeSerialize(opts, callback);
```

### Parameters



Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**OuterComposite**](OuterComposite.md)| Input composite as post body | [optional] 

### Return type

[**OuterComposite**](OuterComposite.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: */*


## fakeOuterNumberSerialize

> Number fakeOuterNumberSerialize(opts)



Test serialization of outer number types

### Example

```javascript
var OpenApiPetstore = require('open_api_petstore');

var apiInstance = new OpenApiPetstore.FakeApi();
var opts = {
  'body': 3.4 // Number | Input number as post body
};
var callback = function(error, data, response) {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully. Returned data: ' + data);
  }
};
apiInstance.fakeOuterNumberSerialize(opts, callback);
```

### Parameters



Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **Number**| Input number as post body | [optional] 

### Return type

**Number**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: */*


## fakeOuterStringSerialize

> String fakeOuterStringSerialize(opts)



Test serialization of outer string types

### Example

```javascript
var OpenApiPetstore = require('open_api_petstore');

var apiInstance = new OpenApiPetstore.FakeApi();
var opts = {
  'body': "body_example" // String | Input string as post body
};
var callback = function(error, data, response) {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully. Returned data: ' + data);
  }
};
apiInstance.fakeOuterStringSerialize(opts, callback);
```

### Parameters



Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **String**| Input string as post body | [optional] 

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: */*


## testBodyWithFileSchema

> testBodyWithFileSchema(body)



For this test, the body for this request much reference a schema named &#x60;File&#x60;.

### Example

```javascript
var OpenApiPetstore = require('open_api_petstore');

var apiInstance = new OpenApiPetstore.FakeApi();
var body = new OpenApiPetstore.FileSchemaTestClass(); // FileSchemaTestClass | 
var callback = function(error, data, response) {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully.');
  }
};
apiInstance.testBodyWithFileSchema(body, callback);
```

### Parameters



Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**FileSchemaTestClass**](FileSchemaTestClass.md)|  | 

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: Not defined


## testBodyWithQueryParams

> testBodyWithQueryParams(query, body)



### Example

```javascript
var OpenApiPetstore = require('open_api_petstore');

var apiInstance = new OpenApiPetstore.FakeApi();
var query = "query_example"; // String | 
var body = new OpenApiPetstore.User(); // User | 
var callback = function(error, data, response) {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully.');
  }
};
apiInstance.testBodyWithQueryParams(query, body, callback);
```

### Parameters



Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **query** | **String**|  | 
 **body** | [**User**](User.md)|  | 

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: Not defined


## testClientModel

> Client testClientModel(body)

To test \&quot;client\&quot; model

To test \&quot;client\&quot; model

### Example

```javascript
var OpenApiPetstore = require('open_api_petstore');

var apiInstance = new OpenApiPetstore.FakeApi();
var body = new OpenApiPetstore.Client(); // Client | client model
var callback = function(error, data, response) {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully. Returned data: ' + data);
  }
};
apiInstance.testClientModel(body, callback);
```

### Parameters



Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Client**](Client.md)| client model | 

### Return type

[**Client**](Client.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json


## testEndpointParameters

> testEndpointParameters(_number, _double, patternWithoutDelimiter, _byte, opts)

Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트

Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트

### Example

```javascript
var OpenApiPetstore = require('open_api_petstore');
var defaultClient = OpenApiPetstore.ApiClient.instance;
// Configure HTTP basic authorization: http_basic_test
var http_basic_test = defaultClient.authentications['http_basic_test'];
http_basic_test.username = 'YOUR USERNAME';
http_basic_test.password = 'YOUR PASSWORD';

var apiInstance = new OpenApiPetstore.FakeApi();
var _number = 3.4; // Number | None
var _double = 3.4; // Number | None
var patternWithoutDelimiter = "patternWithoutDelimiter_example"; // String | None
var _byte = null; // Blob | None
var opts = {
  'integer': 56, // Number | None
  'int32': 56, // Number | None
  'int64': 789, // Number | None
  '_float': 3.4, // Number | None
  '_string': "_string_example", // String | None
  'binary': "/path/to/file", // File | None
  '_date': new Date("2013-10-20"), // Date | None
  'dateTime': new Date("2013-10-20T19:20:30+01:00"), // Date | None
  'password': "password_example", // String | None
  'callback': "callback_example" // String | None
};
var callback = function(error, data, response) {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully.');
  }
};
apiInstance.testEndpointParameters(_number, _double, patternWithoutDelimiter, _byte, opts, callback);
```

### Parameters



Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **_number** | **Number**| None | 
 **_double** | **Number**| None | 
 **patternWithoutDelimiter** | **String**| None | 
 **_byte** | **Blob**| None | 
 **integer** | **Number**| None | [optional] 
 **int32** | **Number**| None | [optional] 
 **int64** | **Number**| None | [optional] 
 **_float** | **Number**| None | [optional] 
 **_string** | **String**| None | [optional] 
 **binary** | **File**| None | [optional] 
 **_date** | **Date**| None | [optional] 
 **dateTime** | **Date**| None | [optional] 
 **password** | **String**| None | [optional] 
 **callback** | **String**| None | [optional] 

### Return type

null (empty response body)

### Authorization

[http_basic_test](../README.md#http_basic_test)

### HTTP request headers

- **Content-Type**: application/x-www-form-urlencoded
- **Accept**: Not defined


## testEnumParameters

> testEnumParameters(opts)

To test enum parameters

To test enum parameters

### Example

```javascript
var OpenApiPetstore = require('open_api_petstore');

var apiInstance = new OpenApiPetstore.FakeApi();
var opts = {
  'enumHeaderStringArray': ["'$'"], // [String] | Header parameter enum test (string array)
  'enumHeaderString': "'-efg'", // String | Header parameter enum test (string)
  'enumQueryStringArray': ["'$'"], // [String] | Query parameter enum test (string array)
  'enumQueryString': "'-efg'", // String | Query parameter enum test (string)
  'enumQueryInteger': 56, // Number | Query parameter enum test (double)
  'enumQueryDouble': 3.4, // Number | Query parameter enum test (double)
  'enumFormStringArray': "'$'", // [String] | Form parameter enum test (string array)
  'enumFormString': "'-efg'" // String | Form parameter enum test (string)
};
var callback = function(error, data, response) {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully.');
  }
};
apiInstance.testEnumParameters(opts, callback);
```

### Parameters



Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **enumHeaderStringArray** | [**[String]**](String.md)| Header parameter enum test (string array) | [optional] 
 **enumHeaderString** | **String**| Header parameter enum test (string) | [optional] [default to &#39;-efg&#39;]
 **enumQueryStringArray** | [**[String]**](String.md)| Query parameter enum test (string array) | [optional] 
 **enumQueryString** | **String**| Query parameter enum test (string) | [optional] [default to &#39;-efg&#39;]
 **enumQueryInteger** | **Number**| Query parameter enum test (double) | [optional] 
 **enumQueryDouble** | **Number**| Query parameter enum test (double) | [optional] 
 **enumFormStringArray** | [**[String]**](String.md)| Form parameter enum test (string array) | [optional] [default to &#39;$&#39;]
 **enumFormString** | **String**| Form parameter enum test (string) | [optional] [default to &#39;-efg&#39;]

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/x-www-form-urlencoded
- **Accept**: Not defined


## testGroupParameters

> testGroupParameters(requiredStringGroup, requiredBooleanGroup, requiredInt64Group, opts)

Fake endpoint to test group parameters (optional)

Fake endpoint to test group parameters (optional)

### Example

```javascript
var OpenApiPetstore = require('open_api_petstore');

var apiInstance = new OpenApiPetstore.FakeApi();
var requiredStringGroup = 56; // Number | Required String in group parameters
var requiredBooleanGroup = true; // Boolean | Required Boolean in group parameters
var requiredInt64Group = 789; // Number | Required Integer in group parameters
var opts = {
  'stringGroup': 56, // Number | String in group parameters
  'booleanGroup': true, // Boolean | Boolean in group parameters
  'int64Group': 789 // Number | Integer in group parameters
};
var callback = function(error, data, response) {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully.');
  }
};
apiInstance.testGroupParameters(requiredStringGroup, requiredBooleanGroup, requiredInt64Group, opts, callback);
```

### Parameters



Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **requiredStringGroup** | **Number**| Required String in group parameters | 
 **requiredBooleanGroup** | **Boolean**| Required Boolean in group parameters | 
 **requiredInt64Group** | **Number**| Required Integer in group parameters | 
 **stringGroup** | **Number**| String in group parameters | [optional] 
 **booleanGroup** | **Boolean**| Boolean in group parameters | [optional] 
 **int64Group** | **Number**| Integer in group parameters | [optional] 

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined


## testInlineAdditionalProperties

> testInlineAdditionalProperties(param)

test inline additionalProperties

### Example

```javascript
var OpenApiPetstore = require('open_api_petstore');

var apiInstance = new OpenApiPetstore.FakeApi();
var param = {key: "null"}; // {String: String} | request body
var callback = function(error, data, response) {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully.');
  }
};
apiInstance.testInlineAdditionalProperties(param, callback);
```

### Parameters



Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **param** | [**{String: String}**](String.md)| request body | 

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: Not defined


## testJsonFormData

> testJsonFormData(param, param2)

test json serialization of form data

### Example

```javascript
var OpenApiPetstore = require('open_api_petstore');

var apiInstance = new OpenApiPetstore.FakeApi();
var param = "param_example"; // String | field1
var param2 = "param2_example"; // String | field2
var callback = function(error, data, response) {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully.');
  }
};
apiInstance.testJsonFormData(param, param2, callback);
```

### Parameters



Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **param** | **String**| field1 | 
 **param2** | **String**| field2 | 

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/x-www-form-urlencoded
- **Accept**: Not defined


## testQueryParameterCollectionFormat

> testQueryParameterCollectionFormat(pipe, ioutil, http, url, context)



To test the collection format in query parameters

### Example

```javascript
var OpenApiPetstore = require('open_api_petstore');

var apiInstance = new OpenApiPetstore.FakeApi();
var pipe = ["null"]; // [String] | 
var ioutil = ["null"]; // [String] | 
var http = ["null"]; // [String] | 
var url = ["null"]; // [String] | 
var context = ["null"]; // [String] | 
var callback = function(error, data, response) {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully.');
  }
};
apiInstance.testQueryParameterCollectionFormat(pipe, ioutil, http, url, context, callback);
```

### Parameters



Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pipe** | [**[String]**](String.md)|  | 
 **ioutil** | [**[String]**](String.md)|  | 
 **http** | [**[String]**](String.md)|  | 
 **url** | [**[String]**](String.md)|  | 
 **context** | [**[String]**](String.md)|  | 

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined

