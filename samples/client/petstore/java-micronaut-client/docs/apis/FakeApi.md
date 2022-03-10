# FakeApi

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
[**testQueryParameterCollectionFormat**](FakeApi.md#testQueryParameterCollectionFormat) | **PUT** /fake/test-query-parameters | 


## Creating FakeApi

To initiate an instance of `FakeApi`, you can use micronaut's `ApplicationContext`:
```java
/* imports
import io.micronaut.runtime.Micronaut;
import io.micronaut.context.ApplicationContext;
import org.openapitools.api.FakeApi;
*/

ApplicationContext context = Micronaut.run(/* ... */);
PetApi apiInstance = context.getBean(FakeApi.class);
```

Or the `@Inject` annotation:
```java
@Singleton
class MyClass {
    @Inject
    FakeApi fakeApi;

    /* ... use the injected variable */
}
```
Note that the class needs to be annotated with one of Micronaut's [scope annotations](https://docs.micronaut.io/latest/guide/#scopes) like `Singleton` in order to be processed.

More information can be found inside [Inversion of Control guide section](https://docs.micronaut.io/latest/guide/#ioc).

<a name="createXmlItem"></a>
# **createXmlItem**
```java
Mono<Void> FakeApi.createXmlItem(xmlItem)
```

creates an XmlItem

this route creates an XmlItem

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **xmlItem** | [**XmlItem**](XmlItem.md)| XmlItem Body |






### HTTP request headers
 - **Content-Type**: `application/xml`, `application/xml; charset=utf-8`, `application/xml; charset=utf-16`, `text/xml`, `text/xml; charset=utf-8`, `text/xml; charset=utf-16`
 - **Accept**: Not defined

<a name="fakeOuterBooleanSerialize"></a>
# **fakeOuterBooleanSerialize**
```java
Mono<Boolean> FakeApi.fakeOuterBooleanSerialize(_body)
```



Test serialization of outer boolean types

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **_body** | `Boolean`| Input boolean as post body | [optional parameter]


### Return type
`Boolean`



### HTTP request headers
 - **Content-Type**: Not defined
 - **Accept**: Not defined

<a name="fakeOuterCompositeSerialize"></a>
# **fakeOuterCompositeSerialize**
```java
Mono<OuterComposite> FakeApi.fakeOuterCompositeSerialize(_body)
```



Test serialization of object with outer number type

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **_body** | [**OuterComposite**](OuterComposite.md)| Input composite as post body | [optional parameter]


### Return type
[**OuterComposite**](OuterComposite.md)



### HTTP request headers
 - **Content-Type**: Not defined
 - **Accept**: Not defined

<a name="fakeOuterNumberSerialize"></a>
# **fakeOuterNumberSerialize**
```java
Mono<BigDecimal> FakeApi.fakeOuterNumberSerialize(_body)
```



Test serialization of outer number types

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **_body** | `BigDecimal`| Input number as post body | [optional parameter]


### Return type
[**BigDecimal**](BigDecimal.md)



### HTTP request headers
 - **Content-Type**: Not defined
 - **Accept**: Not defined

<a name="fakeOuterStringSerialize"></a>
# **fakeOuterStringSerialize**
```java
Mono<String> FakeApi.fakeOuterStringSerialize(_body)
```



Test serialization of outer string types

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **_body** | `String`| Input string as post body | [optional parameter]


### Return type
`String`



### HTTP request headers
 - **Content-Type**: Not defined
 - **Accept**: Not defined

<a name="testBodyWithFileSchema"></a>
# **testBodyWithFileSchema**
```java
Mono<Void> FakeApi.testBodyWithFileSchema(_body)
```



For this test, the body for this request much reference a schema named &#x60;File&#x60;.

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **_body** | [**FileSchemaTestClass**](FileSchemaTestClass.md)|  |






### HTTP request headers
 - **Content-Type**: `application/json`
 - **Accept**: Not defined

<a name="testBodyWithQueryParams"></a>
# **testBodyWithQueryParams**
```java
Mono<Void> FakeApi.testBodyWithQueryParams(query_body)
```



### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **query** | `String`|  |
 **_body** | [**User**](User.md)|  |






### HTTP request headers
 - **Content-Type**: `application/json`
 - **Accept**: Not defined

<a name="testClientModel"></a>
# **testClientModel**
```java
Mono<ModelClient> FakeApi.testClientModel(_body)
```

To test \&quot;client\&quot; model

To test \&quot;client\&quot; model

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **_body** | [**ModelClient**](ModelClient.md)| client model |


### Return type
[**ModelClient**](ModelClient.md)



### HTTP request headers
 - **Content-Type**: `application/json`
 - **Accept**: `application/json`

<a name="testEndpointParameters"></a>
# **testEndpointParameters**
```java
Mono<Void> FakeApi.testEndpointParameters(number_doublepatternWithoutDelimiter_byteintegerint32int64_floatstringbinarydatedateTimepasswordparamCallback)
```

Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트

Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **number** | `BigDecimal`| None |
 **_double** | `Double`| None |
 **patternWithoutDelimiter** | `String`| None |
 **_byte** | `byte[]`| None |
 **integer** | `Integer`| None | [optional parameter]
 **int32** | `Integer`| None | [optional parameter]
 **int64** | `Long`| None | [optional parameter]
 **_float** | `Float`| None | [optional parameter]
 **string** | `String`| None | [optional parameter]
 **binary** | `File`| None | [optional parameter]
 **date** | `LocalDate`| None | [optional parameter]
 **dateTime** | `LocalDateTime`| None | [optional parameter]
 **password** | `String`| None | [optional parameter]
 **paramCallback** | `String`| None | [optional parameter]




### Authorization
* **[http_basic_test](auth.md#http_basic_test)**

### HTTP request headers
 - **Content-Type**: `application/x-www-form-urlencoded`
 - **Accept**: Not defined

<a name="testEnumParameters"></a>
# **testEnumParameters**
```java
Mono<Void> FakeApi.testEnumParameters(enumHeaderStringArrayenumHeaderStringenumQueryStringArrayenumQueryStringenumQueryIntegerenumQueryDoubleenumFormStringArrayenumFormString)
```

To test enum parameters

To test enum parameters

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **enumHeaderStringArray** | [**List&lt;String&gt;**](String.md)| Header parameter enum test (string array) | [optional parameter] [enum: `>`, `$`]
 **enumHeaderString** | `String`| Header parameter enum test (string) | [optional parameter] [default to `-efg`] [enum: `_abc`, `-efg`, `(xyz)`]
 **enumQueryStringArray** | [**List&lt;String&gt;**](String.md)| Query parameter enum test (string array) | [optional parameter] [enum: `>`, `$`]
 **enumQueryString** | `String`| Query parameter enum test (string) | [optional parameter] [default to `-efg`] [enum: `_abc`, `-efg`, `(xyz)`]
 **enumQueryInteger** | `Integer`| Query parameter enum test (double) | [optional parameter] [enum: `1`, `-2`]
 **enumQueryDouble** | `Double`| Query parameter enum test (double) | [optional parameter] [enum: `1.1`, `-1.2`]
 **enumFormStringArray** | [**List&lt;String&gt;**](String.md)| Form parameter enum test (string array) | [optional parameter] [default to `$`] [enum: `>`, `$`]
 **enumFormString** | `String`| Form parameter enum test (string) | [optional parameter] [default to `-efg`] [enum: `_abc`, `-efg`, `(xyz)`]






### HTTP request headers
 - **Content-Type**: `application/x-www-form-urlencoded`
 - **Accept**: Not defined

<a name="testGroupParameters"></a>
# **testGroupParameters**
```java
Mono<Void> FakeApi.testGroupParameters(requiredStringGrouprequiredBooleanGrouprequiredInt64GroupstringGroupbooleanGroupint64Group)
```

Fake endpoint to test group parameters (optional)

Fake endpoint to test group parameters (optional)

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **requiredStringGroup** | `Integer`| Required String in group parameters |
 **requiredBooleanGroup** | `Boolean`| Required Boolean in group parameters |
 **requiredInt64Group** | `Long`| Required Integer in group parameters |
 **stringGroup** | `Integer`| String in group parameters | [optional parameter]
 **booleanGroup** | `Boolean`| Boolean in group parameters | [optional parameter]
 **int64Group** | `Long`| Integer in group parameters | [optional parameter]






### HTTP request headers
 - **Content-Type**: Not defined
 - **Accept**: Not defined

<a name="testInlineAdditionalProperties"></a>
# **testInlineAdditionalProperties**
```java
Mono<Void> FakeApi.testInlineAdditionalProperties(param)
```

test inline additionalProperties

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **param** | [**Map&lt;String, String&gt;**](String.md)| request body |






### HTTP request headers
 - **Content-Type**: `application/json`
 - **Accept**: Not defined

<a name="testJsonFormData"></a>
# **testJsonFormData**
```java
Mono<Void> FakeApi.testJsonFormData(paramparam2)
```

test json serialization of form data

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **param** | `String`| field1 |
 **param2** | `String`| field2 |






### HTTP request headers
 - **Content-Type**: `application/x-www-form-urlencoded`
 - **Accept**: Not defined

<a name="testQueryParameterCollectionFormat"></a>
# **testQueryParameterCollectionFormat**
```java
Mono<Void> FakeApi.testQueryParameterCollectionFormat(pipeioutilhttpurlcontext)
```



To test the collection format in query parameters

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pipe** | [**List&lt;String&gt;**](String.md)|  |
 **ioutil** | [**List&lt;String&gt;**](String.md)|  |
 **http** | [**List&lt;String&gt;**](String.md)|  |
 **url** | [**List&lt;String&gt;**](String.md)|  |
 **context** | [**List&lt;String&gt;**](String.md)|  |






### HTTP request headers
 - **Content-Type**: Not defined
 - **Accept**: Not defined

