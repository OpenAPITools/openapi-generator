# \FakeApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**CreateXmlItem**](FakeApi.md#CreateXmlItem) | **Post** /fake/create_xml_item | creates an XmlItem
[**FakeOuterBooleanSerialize**](FakeApi.md#FakeOuterBooleanSerialize) | **Post** /fake/outer/boolean | 
[**FakeOuterCompositeSerialize**](FakeApi.md#FakeOuterCompositeSerialize) | **Post** /fake/outer/composite | 
[**FakeOuterNumberSerialize**](FakeApi.md#FakeOuterNumberSerialize) | **Post** /fake/outer/number | 
[**FakeOuterStringSerialize**](FakeApi.md#FakeOuterStringSerialize) | **Post** /fake/outer/string | 
[**TestBodyWithFileSchema**](FakeApi.md#TestBodyWithFileSchema) | **Put** /fake/body-with-file-schema | 
[**TestBodyWithQueryParams**](FakeApi.md#TestBodyWithQueryParams) | **Put** /fake/body-with-query-params | 
[**TestClientModel**](FakeApi.md#TestClientModel) | **Patch** /fake | To test \&quot;client\&quot; model
[**TestEndpointParameters**](FakeApi.md#TestEndpointParameters) | **Post** /fake | Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트
[**TestEnumParameters**](FakeApi.md#TestEnumParameters) | **Get** /fake | To test enum parameters
[**TestGroupParameters**](FakeApi.md#TestGroupParameters) | **Delete** /fake | Fake endpoint to test group parameters (optional)
[**TestInlineAdditionalProperties**](FakeApi.md#TestInlineAdditionalProperties) | **Post** /fake/inline-additionalProperties | test inline additionalProperties
[**TestJsonFormData**](FakeApi.md#TestJsonFormData) | **Get** /fake/jsonFormData | test json serialization of form data
[**TestQueryParameterCollectionFormat**](FakeApi.md#TestQueryParameterCollectionFormat) | **Put** /fake/test-query-paramters | 



## CreateXmlItem

> CreateXmlItem(ctx).XmlItem(xmlItem).Execute()

creates an XmlItem



### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiCreateXmlItemRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **xmlItem** | [**XmlItem**](XmlItem.md) | XmlItem Body | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/xml, application/xml; charset=utf-8, application/xml; charset=utf-16, text/xml, text/xml; charset=utf-8, text/xml; charset=utf-16
- **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## FakeOuterBooleanSerialize

> bool FakeOuterBooleanSerialize(ctx).Body(body).Execute()





### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiFakeOuterBooleanSerializeRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **bool** | Input boolean as post body | 

### Return type

**bool**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## FakeOuterCompositeSerialize

> OuterComposite FakeOuterCompositeSerialize(ctx).Body(body).Execute()





### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiFakeOuterCompositeSerializeRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**OuterComposite**](OuterComposite.md) | Input composite as post body | 

### Return type

[**OuterComposite**](OuterComposite.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## FakeOuterNumberSerialize

> float32 FakeOuterNumberSerialize(ctx).Body(body).Execute()





### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiFakeOuterNumberSerializeRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **float32** | Input number as post body | 

### Return type

**float32**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## FakeOuterStringSerialize

> string FakeOuterStringSerialize(ctx).Body(body).Execute()





### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiFakeOuterStringSerializeRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **string** | Input string as post body | 

### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: */*

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## TestBodyWithFileSchema

> TestBodyWithFileSchema(ctx).Body(body).Execute()





### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiTestBodyWithFileSchemaRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**FileSchemaTestClass**](FileSchemaTestClass.md) |  | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## TestBodyWithQueryParams

> TestBodyWithQueryParams(ctx).Query(query).Body(body).Execute()



### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiTestBodyWithQueryParamsRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **query** | **string** |  | 
 **body** | [**User**](User.md) |  | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## TestClientModel

> Client TestClientModel(ctx).Body(body).Execute()

To test \"client\" model



### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiTestClientModelRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Client**](Client.md) | client model | 

### Return type

[**Client**](Client.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## TestEndpointParameters

> TestEndpointParameters(ctx).Number(number).Double(double).PatternWithoutDelimiter(patternWithoutDelimiter).Byte_(byte_).Integer(integer).Int32_(int32_).Int64_(int64_).Float(float).String_(string_).Binary(binary).Date(date).DateTime(dateTime).Password(password).Callback(callback).Execute()

Fake endpoint for testing various parameters  假端點  偽のエンドポイント  가짜 엔드 포인트



### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiTestEndpointParametersRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **number** | **float32** | None | 
 **double** | **float64** | None | 
 **patternWithoutDelimiter** | **string** | None | 
 **byte_** | **string** | None | 
 **integer** | **int32** | None | 
 **int32_** | **int32** | None | 
 **int64_** | **int64** | None | 
 **float** | **float32** | None | 
 **string_** | **string** | None | 
 **binary** | ***os.File** | None | 
 **date** | **string** | None | 
 **dateTime** | **time.Time** | None | 
 **password** | **string** | None | 
 **callback** | **string** | None | 

### Return type

 (empty response body)

### Authorization

[http_basic_test](../README.md#http_basic_test)

### HTTP request headers

- **Content-Type**: application/x-www-form-urlencoded
- **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## TestEnumParameters

> TestEnumParameters(ctx).EnumHeaderStringArray(enumHeaderStringArray).EnumHeaderString(enumHeaderString).EnumQueryStringArray(enumQueryStringArray).EnumQueryString(enumQueryString).EnumQueryInteger(enumQueryInteger).EnumQueryDouble(enumQueryDouble).EnumFormStringArray(enumFormStringArray).EnumFormString(enumFormString).Execute()

To test enum parameters



### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiTestEnumParametersRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **enumHeaderStringArray** | [**[]TestEnumParametersBodyEnumFormStringArrayItems**](TestEnumParametersBodyEnumFormStringArrayItems.md) | Header parameter enum test (string array) | 
 **enumHeaderString** | [**EnumHeaderString**](.md) | Header parameter enum test (string) | 
 **enumQueryStringArray** | [**[]TestEnumParametersBodyEnumFormStringArrayItems**](TestEnumParametersBodyEnumFormStringArrayItems.md) | Query parameter enum test (string array) | 
 **enumQueryString** | [**EnumHeaderString**](.md) | Query parameter enum test (string) | 
 **enumQueryInteger** | [**EnumQueryInteger**](.md) | Query parameter enum test (double) | 
 **enumQueryDouble** | [**EnumQueryDouble**](.md) | Query parameter enum test (double) | 
 **enumFormStringArray** | [**[]TestEnumParametersBodyEnumFormStringArrayItems**](TestEnumParametersBodyEnumFormStringArrayItems.md) | Form parameter enum test (string array) | 
 **enumFormString** | [**TestEnumParametersBodyEnumFormString**](testEnumParametersBodyEnumFormString.md) |  | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/x-www-form-urlencoded
- **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## TestGroupParameters

> TestGroupParameters(ctx).RequiredStringGroup(requiredStringGroup).RequiredBooleanGroup(requiredBooleanGroup).RequiredInt64Group(requiredInt64Group).StringGroup(stringGroup).BooleanGroup(booleanGroup).Int64Group(int64Group).Execute()

Fake endpoint to test group parameters (optional)



### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiTestGroupParametersRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **requiredStringGroup** | **int32** | Required String in group parameters | 
 **requiredBooleanGroup** | **bool** | Required Boolean in group parameters | 
 **requiredInt64Group** | **int64** | Required Integer in group parameters | 
 **stringGroup** | **int32** | String in group parameters | 
 **booleanGroup** | **bool** | Boolean in group parameters | 
 **int64Group** | **int64** | Integer in group parameters | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## TestInlineAdditionalProperties

> TestInlineAdditionalProperties(ctx).Param(param).Execute()

test inline additionalProperties

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiTestInlineAdditionalPropertiesRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **param** | [**map[string]string**](string.md) | request body | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json
- **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## TestJsonFormData

> TestJsonFormData(ctx).Param(param).Param2(param2).Execute()

test json serialization of form data

### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiTestJsonFormDataRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **param** | **string** | field1 | 
 **param2** | **string** | field2 | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/x-www-form-urlencoded
- **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)


## TestQueryParameterCollectionFormat

> TestQueryParameterCollectionFormat(ctx).Pipe(pipe).Ioutil(ioutil).Http(http).Url(url).Context(context).Execute()





### Path Parameters



### Other Parameters

Other parameters are passed through a pointer to a apiTestQueryParameterCollectionFormatRequest struct via the builder pattern


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pipe** | [**[]string**](string.md) |  | 
 **ioutil** | [**[]string**](string.md) |  | 
 **http** | [**[]string**](string.md) |  | 
 **url** | [**[]string**](string.md) |  | 
 **context** | [**[]string**](string.md) |  | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../README.md#documentation-for-models)
[[Back to README]](../README.md)

