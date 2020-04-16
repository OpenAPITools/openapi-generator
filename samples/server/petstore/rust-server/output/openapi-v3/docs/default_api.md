# default_api

All URIs are relative to *http://localhost*

Method | HTTP request | Description
------------- | ------------- | -------------
**CallbackWithHeaderPost**](default_api.md#CallbackWithHeaderPost) | **POST** /callback-with-header | 
**ComplexQueryParamGet**](default_api.md#ComplexQueryParamGet) | **GET** /complex-query-param | 
**EnumInPathPathParamGet**](default_api.md#EnumInPathPathParamGet) | **GET** /enum_in_path/{path_param} | 
**MandatoryRequestHeaderGet**](default_api.md#MandatoryRequestHeaderGet) | **GET** /mandatory-request-header | 
**MergePatchJsonGet**](default_api.md#MergePatchJsonGet) | **GET** /merge-patch-json | 
**MultigetGet**](default_api.md#MultigetGet) | **GET** /multiget | Get some stuff.
**MultipleAuthSchemeGet**](default_api.md#MultipleAuthSchemeGet) | **GET** /multiple_auth_scheme | 
**OverrideServerGet**](default_api.md#OverrideServerGet) | **GET** /override-server | 
**ParamgetGet**](default_api.md#ParamgetGet) | **GET** /paramget | Get some stuff with parameters.
**ReadonlyAuthSchemeGet**](default_api.md#ReadonlyAuthSchemeGet) | **GET** /readonly_auth_scheme | 
**RegisterCallbackPost**](default_api.md#RegisterCallbackPost) | **POST** /register-callback | 
**RequiredOctetStreamPut**](default_api.md#RequiredOctetStreamPut) | **PUT** /required_octet_stream | 
**ResponsesWithHeadersGet**](default_api.md#ResponsesWithHeadersGet) | **GET** /responses_with_headers | 
**Rfc7807Get**](default_api.md#Rfc7807Get) | **GET** /rfc7807 | 
**UntypedPropertyGet**](default_api.md#UntypedPropertyGet) | **GET** /untyped_property | 
**UuidGet**](default_api.md#UuidGet) | **GET** /uuid | 
**XmlExtraPost**](default_api.md#XmlExtraPost) | **POST** /xml_extra | 
**XmlOtherPost**](default_api.md#XmlOtherPost) | **POST** /xml_other | 
**XmlOtherPut**](default_api.md#XmlOtherPut) | **PUT** /xml_other | 
**XmlPost**](default_api.md#XmlPost) | **POST** /xml | Post an array
**XmlPut**](default_api.md#XmlPut) | **PUT** /xml | 


# **CallbackWithHeaderPost**
> CallbackWithHeaderPost(url)


### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
  **url** | **String**|  | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **ComplexQueryParamGet**
> ComplexQueryParamGet(optional)


### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **optional** | **map[string]interface{}** | optional parameters | nil if no parameters

### Optional Parameters
Optional parameters are passed through a map[string]interface{}.

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **list_of_strings** | [**String**](String.md)|  | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **EnumInPathPathParamGet**
> EnumInPathPathParamGet(path_param)


### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
  **path_param** | [****](.md)|  | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **MandatoryRequestHeaderGet**
> MandatoryRequestHeaderGet(x_header)


### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
  **x_header** | **String**|  | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **MergePatchJsonGet**
> models::AnotherXmlObject MergePatchJsonGet()


### Required Parameters
This endpoint does not need any parameter.

### Return type

[**models::AnotherXmlObject**](anotherXmlObject.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/merge-patch+json, 

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **MultigetGet**
> models::AnotherXmlObject MultigetGet()
Get some stuff.

### Required Parameters
This endpoint does not need any parameter.

### Return type

[**models::AnotherXmlObject**](anotherXmlObject.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/octet-stream, application/xml, text/plain, 

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **MultipleAuthSchemeGet**
> MultipleAuthSchemeGet(ctx, )


### Required Parameters
This endpoint does not need any parameter.

### Return type

 (empty response body)

### Authorization

[authScheme](../README.md#authScheme)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **OverrideServerGet**
> OverrideServerGet()


### Required Parameters
This endpoint does not need any parameter.

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **ParamgetGet**
> models::AnotherXmlObject ParamgetGet(optional)
Get some stuff with parameters.

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **optional** | **map[string]interface{}** | optional parameters | nil if no parameters

### Optional Parameters
Optional parameters are passed through a map[string]interface{}.

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **uuid** | [****](.md)| The stuff to get | 
 **some_object** | [****](.md)| Some object to pass as query parameter | 
 **some_list** | [****](.md)| Some list to pass as query parameter | 

### Return type

[**models::AnotherXmlObject**](anotherXmlObject.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, 

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **ReadonlyAuthSchemeGet**
> ReadonlyAuthSchemeGet(ctx, )


### Required Parameters
This endpoint does not need any parameter.

### Return type

 (empty response body)

### Authorization

[authScheme](../README.md#authScheme)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **RegisterCallbackPost**
> RegisterCallbackPost(url)


### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
  **url** | **String**|  | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **RequiredOctetStreamPut**
> RequiredOctetStreamPut(body)


### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
  **body** | **swagger::ByteArray**|  | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/octet-stream
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **ResponsesWithHeadersGet**
> String ResponsesWithHeadersGet()


### Required Parameters
This endpoint does not need any parameter.

### Return type

**String**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, 

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **Rfc7807Get**
> models::ObjectWithArrayOfObjects Rfc7807Get()


### Required Parameters
This endpoint does not need any parameter.

### Return type

[**models::ObjectWithArrayOfObjects**](ObjectWithArrayOfObjects.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/problem+json, application/problem+xml, 

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **UntypedPropertyGet**
> UntypedPropertyGet(optional)


### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **optional** | **map[string]interface{}** | optional parameters | nil if no parameters

### Optional Parameters
Optional parameters are passed through a map[string]interface{}.

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **object_untyped_props** | [**ObjectUntypedProps**](ObjectUntypedProps.md)|  | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **UuidGet**
> uuid::Uuid UuidGet()


### Required Parameters
This endpoint does not need any parameter.

### Return type

[**uuid::Uuid**](UUID.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, 

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **XmlExtraPost**
> XmlExtraPost(optional)


### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **optional** | **map[string]interface{}** | optional parameters | nil if no parameters

### Optional Parameters
Optional parameters are passed through a map[string]interface{}.

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **duplicate_xml_object** | [**DuplicateXmlObject**](DuplicateXmlObject.md)|  | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/xml
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **XmlOtherPost**
> models::AnotherXmlObject XmlOtherPost(optional)


### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **optional** | **map[string]interface{}** | optional parameters | nil if no parameters

### Optional Parameters
Optional parameters are passed through a map[string]interface{}.

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **another_xml_object** | [**AnotherXmlObject**](AnotherXmlObject.md)|  | 

### Return type

[**models::AnotherXmlObject**](anotherXmlObject.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: text/xml
 - **Accept**: text/xml, 

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **XmlOtherPut**
> XmlOtherPut(optional)


### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **optional** | **map[string]interface{}** | optional parameters | nil if no parameters

### Optional Parameters
Optional parameters are passed through a map[string]interface{}.

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **another_xml_array** | [**AnotherXmlArray**](AnotherXmlArray.md)|  | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/xml
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **XmlPost**
> XmlPost(optional)
Post an array

### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **optional** | **map[string]interface{}** | optional parameters | nil if no parameters

### Optional Parameters
Optional parameters are passed through a map[string]interface{}.

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **xml_array** | [**XmlArray**](XmlArray.md)|  | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/xml
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **XmlPut**
> XmlPut(optional)


### Required Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **optional** | **map[string]interface{}** | optional parameters | nil if no parameters

### Optional Parameters
Optional parameters are passed through a map[string]interface{}.

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **xml_object** | [**XmlObject**](XmlObject.md)|  | 

### Return type

 (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/xml
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

