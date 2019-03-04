# petstore_api.PetApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**add_pet**](PetApi.md#add_pet) | **POST** /pet | Add a new pet to the store
[**delete_pet**](PetApi.md#delete_pet) | **DELETE** /pet/{petId} | Deletes a pet
[**find_pets_by_status**](PetApi.md#find_pets_by_status) | **GET** /pet/findByStatus | Finds Pets by status
[**find_pets_by_tags**](PetApi.md#find_pets_by_tags) | **GET** /pet/findByTags | Finds Pets by tags
[**get_pet_by_id**](PetApi.md#get_pet_by_id) | **GET** /pet/{petId} | Find pet by ID
[**update_pet**](PetApi.md#update_pet) | **PUT** /pet | Update an existing pet
[**update_pet_with_form**](PetApi.md#update_pet_with_form) | **POST** /pet/{petId} | Updates a pet in the store with form data
[**upload_file**](PetApi.md#upload_file) | **POST** /pet/{petId}/uploadImage | uploads an image


# **add_pet**
> add_pet(pet)

Add a new pet to the store

### Example
```python
from __future__ import print_function
import time
import petstore_api
from petstore_api import security
from pprint import pprint


# Configure OAuth2 access token for authorization: petstore_auth
# TODO: oauth with 'YOUR_ACCESS_TOKEN'
# If you need multiple auth methods, string them together with MultiAuth
# auth = security.MultiAuth(auth, security.ProxyAuth('PROXY_USERNAME', 'PROXY_PASSWORD'))


# create an instance of the API class
api_client = petstore_api.(auth=auth)
pet = petstore_api.Pet() # Pet | Pet object that needs to be added to the store

try:
    # Add a new pet to the store
    api_client.add_pet(pet)
except ApiException as e:
    print("Exception when calling PetApi->add_pet: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet** | [**Pet**](Pet.md)| Pet object that needs to be added to the store | 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **delete_pet**
> delete_pet(pet_id, api_key=api_key)

Deletes a pet

### Example
```python
from __future__ import print_function
import time
import petstore_api
from petstore_api import security
from pprint import pprint


# Configure OAuth2 access token for authorization: petstore_auth
# TODO: oauth with 'YOUR_ACCESS_TOKEN'
# If you need multiple auth methods, string them together with MultiAuth
# auth = security.MultiAuth(auth, security.ProxyAuth('PROXY_USERNAME', 'PROXY_PASSWORD'))


# create an instance of the API class
api_client = petstore_api.(auth=auth)
pet_id = 56 # int | Pet id to delete
api_key = 'api_key_example' # str |  (optional)

try:
    # Deletes a pet
    api_client.delete_pet(pet_id, api_key=api_key)
except ApiException as e:
    print("Exception when calling PetApi->delete_pet: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet_id** | **int**| Pet id to delete | 
 **api_key** | **str**|  | [optional] 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **find_pets_by_status**
> list[Pet] find_pets_by_status(status)

Finds Pets by status

Multiple status values can be provided with comma separated strings

### Example
```python
from __future__ import print_function
import time
import petstore_api
from petstore_api import security
from pprint import pprint


# Configure OAuth2 access token for authorization: petstore_auth
# TODO: oauth with 'YOUR_ACCESS_TOKEN'
# If you need multiple auth methods, string them together with MultiAuth
# auth = security.MultiAuth(auth, security.ProxyAuth('PROXY_USERNAME', 'PROXY_PASSWORD'))


# create an instance of the API class
api_client = petstore_api.(auth=auth)
status = NULL # list[str] | Status values that need to be considered for filter

try:
    # Finds Pets by status
    api_response = api_client.find_pets_by_status(status)
    pprint(api_response)
except ApiException as e:
    print("Exception when calling PetApi->find_pets_by_status: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **status** | [**list[str]**](str.md)| Status values that need to be considered for filter | 

### Return type

[**list[Pet]**](Pet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **find_pets_by_tags**
> list[Pet] find_pets_by_tags(tags)

Finds Pets by tags

Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.

### Example
```python
from __future__ import print_function
import time
import petstore_api
from petstore_api import security
from pprint import pprint


# Configure OAuth2 access token for authorization: petstore_auth
# TODO: oauth with 'YOUR_ACCESS_TOKEN'
# If you need multiple auth methods, string them together with MultiAuth
# auth = security.MultiAuth(auth, security.ProxyAuth('PROXY_USERNAME', 'PROXY_PASSWORD'))


# create an instance of the API class
api_client = petstore_api.(auth=auth)
tags = NULL # list[str] | Tags to filter by

try:
    # Finds Pets by tags
    api_response = api_client.find_pets_by_tags(tags)
    pprint(api_response)
except ApiException as e:
    print("Exception when calling PetApi->find_pets_by_tags: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **tags** | [**list[str]**](str.md)| Tags to filter by | 

### Return type

[**list[Pet]**](Pet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **get_pet_by_id**
> Pet get_pet_by_id(pet_id)

Find pet by ID

Returns a single pet

### Example
```python
from __future__ import print_function
import time
import petstore_api
from petstore_api import security
from pprint import pprint


# Configure API key authorization: api_key
auth = security.ApiKeySecurity('YOUR_API_KEY')
# If you need multiple auth methods, string them together with MultiAuth
# auth = security.MultiAuth(auth, security.ProxyAuth('PROXY_USERNAME', 'PROXY_PASSWORD'))


# create an instance of the API class
api_client = petstore_api.(auth=auth)
pet_id = 56 # int | ID of pet to return

try:
    # Find pet by ID
    api_response = api_client.get_pet_by_id(pet_id)
    pprint(api_response)
except ApiException as e:
    print("Exception when calling PetApi->get_pet_by_id: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet_id** | **int**| ID of pet to return | 

### Return type

[**Pet**](Pet.md)

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **update_pet**
> update_pet(pet)

Update an existing pet

### Example
```python
from __future__ import print_function
import time
import petstore_api
from petstore_api import security
from pprint import pprint


# Configure OAuth2 access token for authorization: petstore_auth
# TODO: oauth with 'YOUR_ACCESS_TOKEN'
# If you need multiple auth methods, string them together with MultiAuth
# auth = security.MultiAuth(auth, security.ProxyAuth('PROXY_USERNAME', 'PROXY_PASSWORD'))


# create an instance of the API class
api_client = petstore_api.(auth=auth)
pet = petstore_api.Pet() # Pet | Pet object that needs to be added to the store

try:
    # Update an existing pet
    api_client.update_pet(pet)
except ApiException as e:
    print("Exception when calling PetApi->update_pet: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet** | [**Pet**](Pet.md)| Pet object that needs to be added to the store | 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **update_pet_with_form**
> update_pet_with_form(pet_id, name=name, status=status)

Updates a pet in the store with form data

### Example
```python
from __future__ import print_function
import time
import petstore_api
from petstore_api import security
from pprint import pprint


# Configure OAuth2 access token for authorization: petstore_auth
# TODO: oauth with 'YOUR_ACCESS_TOKEN'
# If you need multiple auth methods, string them together with MultiAuth
# auth = security.MultiAuth(auth, security.ProxyAuth('PROXY_USERNAME', 'PROXY_PASSWORD'))


# create an instance of the API class
api_client = petstore_api.(auth=auth)
pet_id = 56 # int | ID of pet that needs to be updated
name = 'name_example' # str | Updated name of the pet (optional)
status = 'status_example' # str | Updated status of the pet (optional)

try:
    # Updates a pet in the store with form data
    api_client.update_pet_with_form(pet_id, name=name, status=status)
except ApiException as e:
    print("Exception when calling PetApi->update_pet_with_form: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet_id** | **int**| ID of pet that needs to be updated | 
 **name** | **str**| Updated name of the pet | [optional] 
 **status** | **str**| Updated status of the pet | [optional] 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **upload_file**
> ApiResponse upload_file(pet_id, additional_metadata=additional_metadata, file=file)

uploads an image

### Example
```python
from __future__ import print_function
import time
import petstore_api
from petstore_api import security
from pprint import pprint


# Configure OAuth2 access token for authorization: petstore_auth
# TODO: oauth with 'YOUR_ACCESS_TOKEN'
# If you need multiple auth methods, string them together with MultiAuth
# auth = security.MultiAuth(auth, security.ProxyAuth('PROXY_USERNAME', 'PROXY_PASSWORD'))


# create an instance of the API class
api_client = petstore_api.(auth=auth)
pet_id = 56 # int | ID of pet to update
additional_metadata = 'additional_metadata_example' # str | Additional data to pass to server (optional)
file = '/path/to/file' # file | file to upload (optional)

try:
    # uploads an image
    api_response = api_client.upload_file(pet_id, additional_metadata=additional_metadata, file=file)
    pprint(api_response)
except ApiException as e:
    print("Exception when calling PetApi->upload_file: %s\n" % e)
```


### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet_id** | **int**| ID of pet to update | 
 **additional_metadata** | **str**| Additional data to pass to server | [optional] 
 **file** | **file**| file to upload | [optional] 

### Return type

[**ApiResponse**](ApiResponse.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

