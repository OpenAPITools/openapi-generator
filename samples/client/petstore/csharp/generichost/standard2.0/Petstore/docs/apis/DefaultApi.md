# Org.OpenAPITools.Api.DefaultApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

| Method | HTTP request | Description |
|--------|--------------|-------------|
| [**FooGet**](DefaultApi.md#fooget) | **GET** /foo |  |
| [**GetCountry**](DefaultApi.md#getcountry) | **POST** /country |  |
| [**Hello**](DefaultApi.md#hello) | **GET** /hello | Hello |
| [**RedirectOrDefault**](DefaultApi.md#redirectordefault) | **GET** /redirectOrDefault |  |
| [**RolesReportGet**](DefaultApi.md#rolesreportget) | **GET** /roles/report |  |
| [**Test**](DefaultApi.md#test) | **GET** /test | Retrieve an existing Notificationtest&#39;s Elements |

<a id="fooget"></a>
# **FooGet**
> FooGetDefaultResponse FooGet ()




### Parameters
This endpoint does not need any parameter.
### Return type

[**FooGetDefaultResponse**](FooGetDefaultResponse.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **0** | response |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

<a id="getcountry"></a>
# **GetCountry**
> void GetCountry (string country)




### Parameters

| Name | Type | Description | Notes |
|------|------|-------------|-------|
| **country** | **string** |  |  |

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | OK |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

<a id="hello"></a>
# **Hello**
> List&lt;Guid&gt; Hello ()

Hello

Hello


### Parameters
This endpoint does not need any parameter.
### Return type

**List<Guid>**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | UUIDs |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

<a id="redirectordefault"></a>
# **RedirectOrDefault**
> string RedirectOrDefault ()




### Parameters
This endpoint does not need any parameter.
### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **301** | redirect |  -  |
| **0** | default |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

<a id="rolesreportget"></a>
# **RolesReportGet**
> List&lt;List&lt;RolesReportsHash&gt;&gt; RolesReportGet ()




### Parameters
This endpoint does not need any parameter.
### Return type

**List<List<RolesReportsHash>>**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | returns report |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

<a id="test"></a>
# **Test**
> NotificationtestGetElementsV1ResponseMPayload Test ()

Retrieve an existing Notificationtest's Elements


### Parameters
This endpoint does not need any parameter.
### Return type

[**NotificationtestGetElementsV1ResponseMPayload**](NotificationtestGetElementsV1ResponseMPayload.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json


### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
| **200** | Successful response |  -  |

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

