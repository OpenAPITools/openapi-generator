# OpenApiPetstore.FakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**fakeHealthGet**](FakeApi.md#fakeHealthGet) | **GET** /fake/health | Health check endpoint
[**fakeHttpSignatureTest**](FakeApi.md#fakeHttpSignatureTest) | **GET** /fake/http-signature-test | test http signature authentication
[**fakeOuterBooleanSerialize**](FakeApi.md#fakeOuterBooleanSerialize) | **POST** /fake/outer/boolean | 
[**fakeOuterCompositeSerialize**](FakeApi.md#fakeOuterCompositeSerialize) | **POST** /fake/outer/composite | 
[**fakeOuterNumberSerialize**](FakeApi.md#fakeOuterNumberSerialize) | **POST** /fake/outer/number | 
[**fakeOuterStringSerialize**](FakeApi.md#fakeOuterStringSerialize) | **POST** /fake/outer/string | 
[**fakePropertyEnumIntegerSerialize**](FakeApi.md#fakePropertyEnumIntegerSerialize) | **POST** /fake/property/enum-int | 
[**testBodyWithBinary**](FakeApi.md#testBodyWithBinary) | **PUT** /fake/body-with-binary | 
[**testBodyWithFileSchema**](FakeApi.md#testBodyWithFileSchema) | **PUT** /fake/body-with-file-schema | 
[**testBodyWithQueryParams**](FakeApi.md#testBodyWithQueryParams) | **PUT** /fake/body-with-query-params | 
[**testClientModel**](FakeApi.md#testClientModel) | **PATCH** /fake | To test \&quot;client\&quot; model
[**testEndpointParameters**](FakeApi.md#testEndpointParameters) | **POST** /fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
[**testEnumParameters**](FakeApi.md#testEnumParameters) | **GET** /fake | To test enum parameters
[**testGroupParameters**](FakeApi.md#testGroupParameters) | **DELETE** /fake | Fake endpoint to test group parameters (optional)
[**testInlineAdditionalProperties**](FakeApi.md#testInlineAdditionalProperties) | **POST** /fake/inline-additionalProperties | test inline additionalProperties
[**testJsonFormData**](FakeApi.md#testJsonFormData) | **GET** /fake/jsonFormData | test json serialization of form data
[**testQueryParameterCollectionFormat**](FakeApi.md#testQueryParameterCollectionFormat) | **PUT** /fake/test-query-parameters | 



## fakeHealthGet

> HealthCheckResult fakeHealthGet()

Health check endpoint

### Example

```javascript
import OpenApiPetstore from 'open_api_petstore';

let apiInstance = new OpenApiPetstore.FakeApi();
apiInstance.fakeHealthGet((error, data, response) => {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully. Returned data: ' + data);
  }
});
```

### Parameters

This endpoint does not need any parameter.

### Return type

[**HealthCheckResult**](HealthCheckResult.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json


## fakeHttpSignatureTest

> fakeHttpSignatureTest(pet, opts)

test http signature authentication

### Example

```javascript
import OpenApiPetstore from 'open_api_petstore';
let defaultClient = OpenApiPetstore.ApiClient.instance;

let apiInstance = new OpenApiPetstore.FakeApi();
let pet = new OpenApiPetstore.Pet(); // Pet | Pet object that needs to be added to the store
let opts = {
  'query1': "query1_example", // String | query parameter
  'header1': "header1_example" // String | header parameter
};
apiInstance.fakeHttpSignatureTest(pet, opts, (error, data, response) => {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully.');
  }
});
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet** | [**Pet**](Pet.md)| Pet object that needs to be added to the store | 
 **query1** | **String**| query parameter | [optional] 
 **header1** | **String**| header parameter | [optional] 

### Return type

null (empty response body)

### Authorization

[http_signature_test](../README.md#http_signature_test)

### HTTP request headers

- **Content-Type**: application/json, application/xml
- **Accept**: Not defined


## fakeOuterBooleanSerialize

> Boolean fakeOuterBooleanSerialize(opts)



Test serialization of outer boolean types

### Example

```javascript
import OpenApiPetstore from 'open_api_petstore';

let apiInstance = new OpenApiPetstore.FakeApi();
let opts = {
  'body': true // Boolean | Input boolean as post body
};
apiInstance.fakeOuterBooleanSerialize(opts, (error, data, response) => {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully. Returned data: ' + data);
  }
});
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

- **Content-Type**: application/json
- **Accept**: */*


## fakeOuterCompositeSerialize

> OuterComposite fakeOuterCompositeSerialize(opts)



Test serialization of object with outer number type

### Example

```javascript
import OpenApiPetstore from 'open_api_petstore';

let apiInstance = new OpenApiPetstore.FakeApi();
let opts = {
  'outerComposite': new OpenApiPetstore.OuterComposite() // OuterComposite | Input composite as post body
};
apiInstance.fakeOuterCompositeSerialize(opts, (error, data, response) => {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully. Returned data: ' + data);
  }
});
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **outerComposite** | [**OuterComposite**](OuterComposite.md)| Input composite as post body | [optional] 

### Return type

[**OuterComposite**](OuterComposite.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: */*


## fakeOuterNumberSerialize

> Number fakeOuterNumberSerialize(opts)



Test serialization of outer number types

### Example

```javascript
import OpenApiPetstore from 'open_api_petstore';

let apiInstance = new OpenApiPetstore.FakeApi();
let opts = {
  'body': 3.4 // Number | Input number as post body
};
apiInstance.fakeOuterNumberSerialize(opts, (error, data, response) => {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully. Returned data: ' + data);
  }
});
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

- **Content-Type**: application/json
- **Accept**: */*


## fakeOuterStringSerialize

> String fakeOuterStringSerialize(opts)



Test serialization of outer string types

### Example

```javascript
import OpenApiPetstore from 'open_api_petstore';

let apiInstance = new OpenApiPetstore.FakeApi();
let opts = {
  'body': "body_example" // String | Input string as post body
};
apiInstance.fakeOuterStringSerialize(opts, (error, data, response) => {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully. Returned data: ' + data);
  }
});
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

- **Content-Type**: application/json
- **Accept**: */*


## fakePropertyEnumIntegerSerialize

> OuterObjectWithEnumProperty fakePropertyEnumIntegerSerialize(outerObjectWithEnumProperty)



Test serialization of enum (int) properties with examples

### Example

```javascript
import OpenApiPetstore from 'open_api_petstore';

let apiInstance = new OpenApiPetstore.FakeApi();
let outerObjectWithEnumProperty = new OpenApiPetstore.OuterObjectWithEnumProperty(); // OuterObjectWithEnumProperty | Input enum (int) as post body
apiInstance.fakePropertyEnumIntegerSerialize(outerObjectWithEnumProperty, (error, data, response) => {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully. Returned data: ' + data);
  }
});
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **outerObjectWithEnumProperty** | [**OuterObjectWithEnumProperty**](OuterObjectWithEnumProperty.md)| Input enum (int) as post body | 

### Return type

[**OuterObjectWithEnumProperty**](OuterObjectWithEnumProperty.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: */*


## testBodyWithBinary

> testBodyWithBinary(body)



For this test, the body has to be a binary file.

### Example

```javascript
import OpenApiPetstore from 'open_api_petstore';

let apiInstance = new OpenApiPetstore.FakeApi();
let body = "/path/to/file"; // File | image to upload
apiInstance.testBodyWithBinary(body, (error, data, response) => {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully.');
  }
});
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **File**| image to upload | 

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: image/png
- **Accept**: Not defined


## testBodyWithFileSchema

> testBodyWithFileSchema(fileSchemaTestClass)



For this test, the body for this request must reference a schema named &#x60;File&#x60;.

### Example

```javascript
import OpenApiPetstore from 'open_api_petstore';

let apiInstance = new OpenApiPetstore.FakeApi();
let fileSchemaTestClass = new OpenApiPetstore.FileSchemaTestClass(); // FileSchemaTestClass | 
apiInstance.testBodyWithFileSchema(fileSchemaTestClass, (error, data, response) => {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully.');
  }
});
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **fileSchemaTestClass** | [**FileSchemaTestClass**](FileSchemaTestClass.md)|  | 

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: Not defined


## testBodyWithQueryParams

> testBodyWithQueryParams(query, user)



### Example

```javascript
import OpenApiPetstore from 'open_api_petstore';

let apiInstance = new OpenApiPetstore.FakeApi();
let query = "query_example"; // String | 
let user = new OpenApiPetstore.User(); // User | 
apiInstance.testBodyWithQueryParams(query, user, (error, data, response) => {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully.');
  }
});
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **query** | **String**|  | 
 **user** | [**User**](User.md)|  | 

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: Not defined


## testClientModel

> Client testClientModel(client)

To test \&quot;client\&quot; model

To test \&quot;client\&quot; model

### Example

```javascript
import OpenApiPetstore from 'open_api_petstore';

let apiInstance = new OpenApiPetstore.FakeApi();
let client = new OpenApiPetstore.Client(); // Client | client model
apiInstance.testClientModel(client, (error, data, response) => {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully. Returned data: ' + data);
  }
});
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **client** | [**Client**](Client.md)| client model | 

### Return type

[**Client**](Client.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json


## testEndpointParameters

> testEndpointParameters(number, _double, patternWithoutDelimiter, _byte, opts)

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 

### Example

```javascript
import OpenApiPetstore from 'open_api_petstore';
let defaultClient = OpenApiPetstore.ApiClient.instance;
// Configure HTTP basic authorization: http_basic_test
let http_basic_test = defaultClient.authentications['http_basic_test'];
http_basic_test.username = 'YOUR USERNAME';
http_basic_test.password = 'YOUR PASSWORD';

let apiInstance = new OpenApiPetstore.FakeApi();
let number = 3.4; // Number | None
let _double = 3.4; // Number | None
let patternWithoutDelimiter = "patternWithoutDelimiter_example"; // String | None
let _byte = null; // Blob | None
let opts = {
  'integer': 56, // Number | None
  'int32': 56, // Number | None
  'int64': 789, // Number | None
  '_float': 3.4, // Number | None
  'string': "string_example", // String | None
  'binary': "/path/to/file", // File | None
  'date': new Date("2013-10-20"), // Date | None
  'dateTime': new Date("2013-10-20T19:20:30+01:00"), // Date | None
  'password': "password_example", // String | None
  'callback': "callback_example" // String | None
};
apiInstance.testEndpointParameters(number, _double, patternWithoutDelimiter, _byte, opts, (error, data, response) => {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully.');
  }
});
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **number** | **Number**| None | 
 **_double** | **Number**| None | 
 **patternWithoutDelimiter** | **String**| None | 
 **_byte** | **Blob**| None | 
 **integer** | **Number**| None | [optional] 
 **int32** | **Number**| None | [optional] 
 **int64** | **Number**| None | [optional] 
 **_float** | **Number**| None | [optional] 
 **string** | **String**| None | [optional] 
 **binary** | **File**| None | [optional] 
 **date** | **Date**| None | [optional] 
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
import OpenApiPetstore from 'open_api_petstore';

let apiInstance = new OpenApiPetstore.FakeApi();
let opts = {
  'enumHeaderStringArray': ["'$'"], // [String] | Header parameter enum test (string array)
  'enumHeaderString': "'-efg'", // String | Header parameter enum test (string)
  'enumQueryStringArray': ["'$'"], // [String] | Query parameter enum test (string array)
  'enumQueryString': "'-efg'", // String | Query parameter enum test (string)
  'enumQueryInteger': 56, // Number | Query parameter enum test (double)
  'enumQueryDouble': 3.4, // Number | Query parameter enum test (double)
  'enumFormStringArray': ["'$'"], // [String] | Form parameter enum test (string array)
  'enumFormString': "'-efg'" // String | Form parameter enum test (string)
};
apiInstance.testEnumParameters(opts, (error, data, response) => {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully.');
  }
});
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
import OpenApiPetstore from 'open_api_petstore';
let defaultClient = OpenApiPetstore.ApiClient.instance;
// Configure Bearer (JWT) access token for authorization: bearer_test
let bearer_test = defaultClient.authentications['bearer_test'];
bearer_test.accessToken = "YOUR ACCESS TOKEN"

let apiInstance = new OpenApiPetstore.FakeApi();
let requiredStringGroup = 56; // Number | Required String in group parameters
let requiredBooleanGroup = true; // Boolean | Required Boolean in group parameters
let requiredInt64Group = 789; // Number | Required Integer in group parameters
let opts = {
  'stringGroup': 56, // Number | String in group parameters
  'booleanGroup': true, // Boolean | Boolean in group parameters
  'int64Group': 789 // Number | Integer in group parameters
};
apiInstance.testGroupParameters(requiredStringGroup, requiredBooleanGroup, requiredInt64Group, opts, (error, data, response) => {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully.');
  }
});
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

[bearer_test](../README.md#bearer_test)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined


## testInlineAdditionalProperties

> testInlineAdditionalProperties(requestBody)

test inline additionalProperties



### Example

```javascript
import OpenApiPetstore from 'open_api_petstore';

let apiInstance = new OpenApiPetstore.FakeApi();
let requestBody = {key: "null"}; // {String: String} | request body
apiInstance.testInlineAdditionalProperties(requestBody, (error, data, response) => {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully.');
  }
});
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **requestBody** | [**{String: String}**](String.md)| request body | 

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
import OpenApiPetstore from 'open_api_petstore';

let apiInstance = new OpenApiPetstore.FakeApi();
let param = "param_example"; // String | field1
let param2 = "param2_example"; // String | field2
apiInstance.testJsonFormData(param, param2, (error, data, response) => {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully.');
  }
});
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

> testQueryParameterCollectionFormat(pipe, ioutil, http, url, context, allowEmpty, opts)



To test the collection format in query parameters

### Example

```javascript
import OpenApiPetstore from 'open_api_petstore';

let apiInstance = new OpenApiPetstore.FakeApi();
let pipe = ["null"]; // [String] | 
let ioutil = ["null"]; // [String] | 
let http = ["null"]; // [String] | 
let url = ["null"]; // [String] | 
let context = ["null"]; // [String] | 
let allowEmpty = "allowEmpty_example"; // String | 
let opts = {
  'language': {key: "null"} // {String: String} | 
};
apiInstance.testQueryParameterCollectionFormat(pipe, ioutil, http, url, context, allowEmpty, opts, (error, data, response) => {
  if (error) {
    console.error(error);
  } else {
    console.log('API called successfully.');
  }
});
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pipe** | [**[String]**](String.md)|  | 
 **ioutil** | [**[String]**](String.md)|  | 
 **http** | [**[String]**](String.md)|  | 
 **url** | [**[String]**](String.md)|  | 
 **context** | [**[String]**](String.md)|  | 
 **allowEmpty** | **String**|  | 
 **language** | [**{String: String}**](String.md)|  | [optional] 

### Return type

null (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined

