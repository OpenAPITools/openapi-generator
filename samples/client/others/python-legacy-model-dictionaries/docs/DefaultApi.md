# legacy_model_dict_client.DefaultApi

All URIs are relative to *http://localhost*

Method | HTTP request | Description
------------- | ------------- | -------------
[**list_legacy_models**](DefaultApi.md#list_legacy_models) | **GET** /legacy-models | List legacy models


# **list_legacy_models**
> list[LegacyModel] list_legacy_models()

List legacy models

### Example


```python
import legacy_model_dict_client
from legacy_model_dict_client.models.legacy_model import LegacyModel
from legacy_model_dict_client.rest import ApiException
from pprint import pprint

# Defining the host is optional and defaults to http://localhost
# See configuration.py for a list of all supported configuration parameters.
configuration = legacy_model_dict_client.Configuration(
    host = "http://localhost"
)


# Enter a context with an instance of the API client
with legacy_model_dict_client.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = legacy_model_dict_client.DefaultApi(api_client)

    try:
        # List legacy models
        api_response = api_instance.list_legacy_models()
        print("The response of DefaultApi->list_legacy_models:\n")
        pprint(api_response)
    except Exception as e:
        print("Exception when calling DefaultApi->list_legacy_models: %s\n" % e)
```



### Parameters

This endpoint does not need any parameter.

### Return type

[**list[LegacyModel]**](LegacyModel.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

### HTTP response details

| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | Legacy model list |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

