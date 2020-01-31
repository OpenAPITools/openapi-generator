# petstore_api.PetApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

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
[**upload_file_with_required_file**](PetApi.md#upload_file_with_required_file) | **POST** /fake/{petId}/uploadImageWithRequiredFile | uploads an image (required)


# **add_pet**
> add_pet(pet_pet)

Add a new pet to the store

### Example

* OAuth Authentication (petstore_auth):
```python
from __future__ import print_function
import time
import petstore_api
from pprint import pprint
configuration = petstore_api.Configuration()
# Configure HTTP signature authorization: http_signature_test
# You can specify the signing key-id, private key path, signing scheme, signing algorithm,
# list of signed headers and signature max validity.
configuration.signing_info = petstore_api.signing.HttpSigningConfiguration(
    key_id =                 'my-key-id',
    private_key_path =       'rsa.pem',
    signing_scheme =         signing.SCHEME_HS2019,
    signing_algorithm =      signing.ALGORITHM_RSASSA_PSS,
    signed_headers =         [signing.HEADER_REQUEST_TARGET,
                                signing.HEADER_CREATED,
                                signing.HEADER_EXPIRES,
                                signing.HEADER_HOST,
                                signing.HEADER_DATE,
                                signing.HEADER_DIGEST,
                                'Content-Type',
                                'Content-Length',
                                'User-Agent'
                                ],
    signature_max_validity = datetime.timedelta(minutes=5)
)
configuration = petstore_api.Configuration()
# Configure OAuth2 access token for authorization: petstore_auth
configuration.access_token = 'YOUR_ACCESS_TOKEN'

# Defining host is optional and default to http://petstore.swagger.io:80/v2
configuration.host = "http://petstore.swagger.io:80/v2"
# Enter a context with an instance of the API client
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = petstore_api.PetApi(api_client)
    pet_pet = petstore_api.Pet() # pet.Pet | Pet object that needs to be added to the store
    
    # example passing only required values which don't have defaults set
    try:
        # Add a new pet to the store
        api_instance.add_pet(pet_pet)
    except petstore_api.ApiException as e:
        print("Exception when calling PetApi->add_pet: %s\n" % e)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet_pet** | [**pet.Pet**](Pet.md)| Pet object that needs to be added to the store |

### Return type

void (empty response body)

### Authorization

[http_signature_test](../README.md#http_signature_test), [petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: Not defined

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**405** | Invalid input |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **delete_pet**
> delete_pet(pet_id)

Deletes a pet

### Example

* OAuth Authentication (petstore_auth):
```python
from __future__ import print_function
import time
import petstore_api
from pprint import pprint
configuration = petstore_api.Configuration()
# Configure OAuth2 access token for authorization: petstore_auth
configuration.access_token = 'YOUR_ACCESS_TOKEN'

# Defining host is optional and default to http://petstore.swagger.io:80/v2
configuration.host = "http://petstore.swagger.io:80/v2"
# Enter a context with an instance of the API client
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = petstore_api.PetApi(api_client)
    pet_id = 56 # int | Pet id to delete
    api_key = 'api_key_example' # str |  (optional)

    # example passing only required values which don't have defaults set
    try:
        # Deletes a pet
        api_instance.delete_pet(pet_id)
    except petstore_api.ApiException as e:
        print("Exception when calling PetApi->delete_pet: %s\n" % e)

    # example passing only required values which don't have defaults set
    # and optional values
    try:
        # Deletes a pet
        api_instance.delete_pet(pet_id, api_key=api_key)
    except petstore_api.ApiException as e:
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

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**400** | Invalid pet value |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **find_pets_by_status**
> [pet.Pet] find_pets_by_status(status)

Finds Pets by status

Multiple status values can be provided with comma separated strings

### Example

* OAuth Authentication (petstore_auth):
```python
from __future__ import print_function
import time
import petstore_api
from pprint import pprint
configuration = petstore_api.Configuration()
# Configure HTTP signature authorization: http_signature_test
# You can specify the signing key-id, private key path, signing scheme, signing algorithm,
# list of signed headers and signature max validity.
configuration.signing_info = petstore_api.signing.HttpSigningConfiguration(
    key_id =                 'my-key-id',
    private_key_path =       'rsa.pem',
    signing_scheme =         signing.SCHEME_HS2019,
    signing_algorithm =      signing.ALGORITHM_RSASSA_PSS,
    signed_headers =         [signing.HEADER_REQUEST_TARGET,
                                signing.HEADER_CREATED,
                                signing.HEADER_EXPIRES,
                                signing.HEADER_HOST,
                                signing.HEADER_DATE,
                                signing.HEADER_DIGEST,
                                'Content-Type',
                                'Content-Length',
                                'User-Agent'
                                ],
    signature_max_validity = datetime.timedelta(minutes=5)
)
configuration = petstore_api.Configuration()
# Configure OAuth2 access token for authorization: petstore_auth
configuration.access_token = 'YOUR_ACCESS_TOKEN'

# Defining host is optional and default to http://petstore.swagger.io:80/v2
configuration.host = "http://petstore.swagger.io:80/v2"
# Enter a context with an instance of the API client
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = petstore_api.PetApi(api_client)
    status = ['status_example'] # [str] | Status values that need to be considered for filter
    
    # example passing only required values which don't have defaults set
    try:
        # Finds Pets by status
        api_response = api_instance.find_pets_by_status(status)
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling PetApi->find_pets_by_status: %s\n" % e)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **status** | **[str]**| Status values that need to be considered for filter |

### Return type

[**[pet.Pet]**](Pet.md)

### Authorization

[http_signature_test](../README.md#http_signature_test), [petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | successful operation |  -  |
**400** | Invalid status value |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **find_pets_by_tags**
> [pet.Pet] find_pets_by_tags(tags)

Finds Pets by tags

Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.

### Example

* OAuth Authentication (petstore_auth):
```python
from __future__ import print_function
import time
import petstore_api
from pprint import pprint
configuration = petstore_api.Configuration()
# Configure HTTP signature authorization: http_signature_test
# You can specify the signing key-id, private key path, signing scheme, signing algorithm,
# list of signed headers and signature max validity.
configuration.signing_info = petstore_api.signing.HttpSigningConfiguration(
    key_id =                 'my-key-id',
    private_key_path =       'rsa.pem',
    signing_scheme =         signing.SCHEME_HS2019,
    signing_algorithm =      signing.ALGORITHM_RSASSA_PSS,
    signed_headers =         [signing.HEADER_REQUEST_TARGET,
                                signing.HEADER_CREATED,
                                signing.HEADER_EXPIRES,
                                signing.HEADER_HOST,
                                signing.HEADER_DATE,
                                signing.HEADER_DIGEST,
                                'Content-Type',
                                'Content-Length',
                                'User-Agent'
                                ],
    signature_max_validity = datetime.timedelta(minutes=5)
)
configuration = petstore_api.Configuration()
# Configure OAuth2 access token for authorization: petstore_auth
configuration.access_token = 'YOUR_ACCESS_TOKEN'

# Defining host is optional and default to http://petstore.swagger.io:80/v2
configuration.host = "http://petstore.swagger.io:80/v2"
# Enter a context with an instance of the API client
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = petstore_api.PetApi(api_client)
    tags = ['tags_example'] # [str] | Tags to filter by
    
    # example passing only required values which don't have defaults set
    try:
        # Finds Pets by tags
        api_response = api_instance.find_pets_by_tags(tags)
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling PetApi->find_pets_by_tags: %s\n" % e)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **tags** | **[str]**| Tags to filter by |

### Return type

[**[pet.Pet]**](Pet.md)

### Authorization

[http_signature_test](../README.md#http_signature_test), [petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | successful operation |  -  |
**400** | Invalid tag value |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **get_pet_by_id**
> pet.Pet get_pet_by_id(pet_id)

Find pet by ID

Returns a single pet

### Example

* Api Key Authentication (api_key):
```python
from __future__ import print_function
import time
import petstore_api
from pprint import pprint
configuration = petstore_api.Configuration()
# Configure API key authorization: api_key
configuration.api_key['api_key'] = 'YOUR_API_KEY'
# Uncomment below to setup prefix (e.g. Bearer) for API key, if needed
# configuration.api_key_prefix['api_key'] = 'Bearer'

# Defining host is optional and default to http://petstore.swagger.io:80/v2
configuration.host = "http://petstore.swagger.io:80/v2"
# Enter a context with an instance of the API client
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = petstore_api.PetApi(api_client)
    pet_id = 56 # int | ID of pet to return
    
    # example passing only required values which don't have defaults set
    try:
        # Find pet by ID
        api_response = api_instance.get_pet_by_id(pet_id)
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling PetApi->get_pet_by_id: %s\n" % e)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet_id** | **int**| ID of pet to return |

### Return type

[**pet.Pet**](Pet.md)

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | successful operation |  -  |
**400** | Invalid ID supplied |  -  |
**404** | Pet not found |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **update_pet**
> update_pet(pet_pet)

Update an existing pet

### Example

* OAuth Authentication (petstore_auth):
```python
from __future__ import print_function
import time
import petstore_api
from pprint import pprint
configuration = petstore_api.Configuration()
# Configure HTTP signature authorization: http_signature_test
# You can specify the signing key-id, private key path, signing scheme, signing algorithm,
# list of signed headers and signature max validity.
configuration.signing_info = petstore_api.signing.HttpSigningConfiguration(
    key_id =                 'my-key-id',
    private_key_path =       'rsa.pem',
    signing_scheme =         signing.SCHEME_HS2019,
    signing_algorithm =      signing.ALGORITHM_RSASSA_PSS,
    signed_headers =         [signing.HEADER_REQUEST_TARGET,
                                signing.HEADER_CREATED,
                                signing.HEADER_EXPIRES,
                                signing.HEADER_HOST,
                                signing.HEADER_DATE,
                                signing.HEADER_DIGEST,
                                'Content-Type',
                                'Content-Length',
                                'User-Agent'
                                ],
    signature_max_validity = datetime.timedelta(minutes=5)
)
configuration = petstore_api.Configuration()
# Configure OAuth2 access token for authorization: petstore_auth
configuration.access_token = 'YOUR_ACCESS_TOKEN'

# Defining host is optional and default to http://petstore.swagger.io:80/v2
configuration.host = "http://petstore.swagger.io:80/v2"
# Enter a context with an instance of the API client
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = petstore_api.PetApi(api_client)
    pet_pet = petstore_api.Pet() # pet.Pet | Pet object that needs to be added to the store
    
    # example passing only required values which don't have defaults set
    try:
        # Update an existing pet
        api_instance.update_pet(pet_pet)
    except petstore_api.ApiException as e:
        print("Exception when calling PetApi->update_pet: %s\n" % e)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet_pet** | [**pet.Pet**](Pet.md)| Pet object that needs to be added to the store |

### Return type

void (empty response body)

### Authorization

[http_signature_test](../README.md#http_signature_test), [petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: Not defined

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**400** | Invalid ID supplied |  -  |
**404** | Pet not found |  -  |
**405** | Validation exception |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **update_pet_with_form**
> update_pet_with_form(pet_id)

Updates a pet in the store with form data

### Example

* OAuth Authentication (petstore_auth):
```python
from __future__ import print_function
import time
import petstore_api
from pprint import pprint
configuration = petstore_api.Configuration()
# Configure OAuth2 access token for authorization: petstore_auth
configuration.access_token = 'YOUR_ACCESS_TOKEN'

# Defining host is optional and default to http://petstore.swagger.io:80/v2
configuration.host = "http://petstore.swagger.io:80/v2"
# Enter a context with an instance of the API client
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = petstore_api.PetApi(api_client)
    pet_id = 56 # int | ID of pet that needs to be updated
    name = 'name_example' # str | Updated name of the pet (optional)
status = 'status_example' # str | Updated status of the pet (optional)

    # example passing only required values which don't have defaults set
    try:
        # Updates a pet in the store with form data
        api_instance.update_pet_with_form(pet_id)
    except petstore_api.ApiException as e:
        print("Exception when calling PetApi->update_pet_with_form: %s\n" % e)

    # example passing only required values which don't have defaults set
    # and optional values
    try:
        # Updates a pet in the store with form data
        api_instance.update_pet_with_form(pet_id, name=name, status=status)
    except petstore_api.ApiException as e:
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

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**405** | Invalid input |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **upload_file**
> api_response.ApiResponse upload_file(pet_id)

uploads an image

### Example

* OAuth Authentication (petstore_auth):
```python
from __future__ import print_function
import time
import petstore_api
from pprint import pprint
configuration = petstore_api.Configuration()
# Configure OAuth2 access token for authorization: petstore_auth
configuration.access_token = 'YOUR_ACCESS_TOKEN'

# Defining host is optional and default to http://petstore.swagger.io:80/v2
configuration.host = "http://petstore.swagger.io:80/v2"
# Enter a context with an instance of the API client
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = petstore_api.PetApi(api_client)
    pet_id = 56 # int | ID of pet to update
    additional_metadata = 'additional_metadata_example' # str | Additional data to pass to server (optional)
file = open('/path/to/file', 'rb') # file_type | file to upload (optional)

    # example passing only required values which don't have defaults set
    try:
        # uploads an image
        api_response = api_instance.upload_file(pet_id)
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling PetApi->upload_file: %s\n" % e)

    # example passing only required values which don't have defaults set
    # and optional values
    try:
        # uploads an image
        api_response = api_instance.upload_file(pet_id, additional_metadata=additional_metadata, file=file)
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling PetApi->upload_file: %s\n" % e)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet_id** | **int**| ID of pet to update |
 **additional_metadata** | **str**| Additional data to pass to server | [optional]
 **file** | **file_type**| file to upload | [optional]

### Return type

[**api_response.ApiResponse**](ApiResponse.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **upload_file_with_required_file**
> api_response.ApiResponse upload_file_with_required_file(pet_id, required_file)

uploads an image (required)

### Example

* OAuth Authentication (petstore_auth):
```python
from __future__ import print_function
import time
import petstore_api
from pprint import pprint
configuration = petstore_api.Configuration()
# Configure OAuth2 access token for authorization: petstore_auth
configuration.access_token = 'YOUR_ACCESS_TOKEN'

# Defining host is optional and default to http://petstore.swagger.io:80/v2
configuration.host = "http://petstore.swagger.io:80/v2"
# Enter a context with an instance of the API client
with petstore_api.ApiClient(configuration) as api_client:
    # Create an instance of the API class
    api_instance = petstore_api.PetApi(api_client)
    pet_id = 56 # int | ID of pet to update
    required_file = open('/path/to/file', 'rb') # file_type | file to upload
    additional_metadata = 'additional_metadata_example' # str | Additional data to pass to server (optional)

    # example passing only required values which don't have defaults set
    try:
        # uploads an image (required)
        api_response = api_instance.upload_file_with_required_file(pet_id, required_file)
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling PetApi->upload_file_with_required_file: %s\n" % e)

    # example passing only required values which don't have defaults set
    # and optional values
    try:
        # uploads an image (required)
        api_response = api_instance.upload_file_with_required_file(pet_id, required_file, additional_metadata=additional_metadata)
        pprint(api_response)
    except petstore_api.ApiException as e:
        print("Exception when calling PetApi->upload_file_with_required_file: %s\n" % e)
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet_id** | **int**| ID of pet to update |
 **required_file** | **file_type**| file to upload |
 **additional_metadata** | **str**| Additional data to pass to server | [optional]

### Return type

[**api_response.ApiResponse**](ApiResponse.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: application/json

### HTTP response details
| Status code | Description | Response headers |
|-------------|-------------|------------------|
**200** | successful operation |  -  |

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

