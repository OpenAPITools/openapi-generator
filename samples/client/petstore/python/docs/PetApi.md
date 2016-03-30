# swagger_client\PetApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**add_pet**](PetApi.md#add_pet) | **POST** /pet | Add a new pet to the store
[**add_pet_using_byte_array**](PetApi.md#add_pet_using_byte_array) | **POST** /pet?testing_byte_array=true | Fake endpoint to test byte array in body parameter for adding a new pet to the store
[**delete_pet**](PetApi.md#delete_pet) | **DELETE** /pet/{petId} | Deletes a pet
[**find_pets_by_status**](PetApi.md#find_pets_by_status) | **GET** /pet/findByStatus | Finds Pets by status
[**find_pets_by_tags**](PetApi.md#find_pets_by_tags) | **GET** /pet/findByTags | Finds Pets by tags
[**get_pet_by_id**](PetApi.md#get_pet_by_id) | **GET** /pet/{petId} | Find pet by ID
[**get_pet_by_id_in_object**](PetApi.md#get_pet_by_id_in_object) | **GET** /pet/{petId}?response=inline_arbitrary_object | Fake endpoint to test inline arbitrary object return by &#39;Find pet by ID&#39;
[**pet_pet_idtesting_byte_arraytrue_get**](PetApi.md#pet_pet_idtesting_byte_arraytrue_get) | **GET** /pet/{petId}?testing_byte_array=true | Fake endpoint to test byte array return by &#39;Find pet by ID&#39;
[**update_pet**](PetApi.md#update_pet) | **PUT** /pet | Update an existing pet
[**update_pet_with_form**](PetApi.md#update_pet_with_form) | **POST** /pet/{petId} | Updates a pet in the store with form data
[**upload_file**](PetApi.md#upload_file) | **POST** /pet/{petId}/uploadImage | uploads an image


# **add_pet**
> add_pet(body=body)

Add a new pet to the store



### Example 
```python
import swagger_client
from swagger_client.rest import ApiException
from pprint import pprint
import time


# Configure OAuth2 access token for authorization: petstore_auth
swagger_client.configuration.access_token = 'YOUR_ACCESS_TOKEN'

# create an instance of the API class
api_instance = swagger_client.PetApi()
body = swagger_client.Pet() # Pet | Pet object that needs to be added to the store (optional)

try: 
    # Add a new pet to the store
    api_instance.add_pet(body=body);
except ApiException as e:
    print "Exception when calling PetApi->add_pet: %s\n" % e
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Pet**](Pet.md)| Pet object that needs to be added to the store | [optional] 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP reuqest headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **add_pet_using_byte_array**
> add_pet_using_byte_array(body=body)

Fake endpoint to test byte array in body parameter for adding a new pet to the store



### Example 
```python
import swagger_client
from swagger_client.rest import ApiException
from pprint import pprint
import time


# Configure OAuth2 access token for authorization: petstore_auth
swagger_client.configuration.access_token = 'YOUR_ACCESS_TOKEN'

# create an instance of the API class
api_instance = swagger_client.PetApi()
body = 'B' # str | Pet object in the form of byte array (optional)

try: 
    # Fake endpoint to test byte array in body parameter for adding a new pet to the store
    api_instance.add_pet_using_byte_array(body=body);
except ApiException as e:
    print "Exception when calling PetApi->add_pet_using_byte_array: %s\n" % e
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **str**| Pet object in the form of byte array | [optional] 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP reuqest headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **delete_pet**
> delete_pet(pet_id, api_key=api_key)

Deletes a pet



### Example 
```python
import swagger_client
from swagger_client.rest import ApiException
from pprint import pprint
import time


# Configure OAuth2 access token for authorization: petstore_auth
swagger_client.configuration.access_token = 'YOUR_ACCESS_TOKEN'

# create an instance of the API class
api_instance = swagger_client.PetApi()
pet_id = 789 # int | Pet id to delete
api_key = 'api_key_example' # str |  (optional)

try: 
    # Deletes a pet
    api_instance.delete_pet(pet_id, api_key=api_key);
except ApiException as e:
    print "Exception when calling PetApi->delete_pet: %s\n" % e
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

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **find_pets_by_status**
> list[Pet] find_pets_by_status(status=status)

Finds Pets by status

Multiple status values can be provided with comma separated strings

### Example 
```python
import swagger_client
from swagger_client.rest import ApiException
from pprint import pprint
import time


# Configure OAuth2 access token for authorization: petstore_auth
swagger_client.configuration.access_token = 'YOUR_ACCESS_TOKEN'

# create an instance of the API class
api_instance = swagger_client.PetApi()
status = ['available'] # list[str] | Status values that need to be considered for query (optional) (default to available)

try: 
    # Finds Pets by status
    api_response = api_instance.find_pets_by_status(status=status);
    pprint(api_response)
except ApiException as e:
    print "Exception when calling PetApi->find_pets_by_status: %s\n" % e
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **status** | [**list[str]**](str.md)| Status values that need to be considered for query | [optional] [default to available]

### Return type

[**list[Pet]**](Pet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **find_pets_by_tags**
> list[Pet] find_pets_by_tags(tags=tags)

Finds Pets by tags

Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.

### Example 
```python
import swagger_client
from swagger_client.rest import ApiException
from pprint import pprint
import time


# Configure OAuth2 access token for authorization: petstore_auth
swagger_client.configuration.access_token = 'YOUR_ACCESS_TOKEN'

# create an instance of the API class
api_instance = swagger_client.PetApi()
tags = ['tags_example'] # list[str] | Tags to filter by (optional)

try: 
    # Finds Pets by tags
    api_response = api_instance.find_pets_by_tags(tags=tags);
    pprint(api_response)
except ApiException as e:
    print "Exception when calling PetApi->find_pets_by_tags: %s\n" % e
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **tags** | [**list[str]**](str.md)| Tags to filter by | [optional] 

### Return type

[**list[Pet]**](Pet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **get_pet_by_id**
> Pet get_pet_by_id(pet_id)

Find pet by ID

Returns a pet when ID < 10.  ID > 10 or nonintegers will simulate API error conditions

### Example 
```python
import swagger_client
from swagger_client.rest import ApiException
from pprint import pprint
import time


# Configure API key authorization: api_key
swagger_client.configuration.api_key['api_key'] = 'YOUR_API_KEY';
# Uncomment below to setup prefix (e.g. BEARER) for API key, if needed
# swagger_client.configuration.api_key_prefix['api_key'] = 'BEARER'
# Configure OAuth2 access token for authorization: petstore_auth
swagger_client.configuration.access_token = 'YOUR_ACCESS_TOKEN'

# create an instance of the API class
api_instance = swagger_client.PetApi()
pet_id = 789 # int | ID of pet that needs to be fetched

try: 
    # Find pet by ID
    api_response = api_instance.get_pet_by_id(pet_id);
    pprint(api_response)
except ApiException as e:
    print "Exception when calling PetApi->get_pet_by_id: %s\n" % e
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet_id** | **int**| ID of pet that needs to be fetched | 

### Return type

[**Pet**](Pet.md)

### Authorization

[api_key](../README.md#api_key), [petstore_auth](../README.md#petstore_auth)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **get_pet_by_id_in_object**
> InlineResponse200 get_pet_by_id_in_object(pet_id)

Fake endpoint to test inline arbitrary object return by 'Find pet by ID'

Returns a pet when ID < 10.  ID > 10 or nonintegers will simulate API error conditions

### Example 
```python
import swagger_client
from swagger_client.rest import ApiException
from pprint import pprint
import time


# Configure API key authorization: api_key
swagger_client.configuration.api_key['api_key'] = 'YOUR_API_KEY';
# Uncomment below to setup prefix (e.g. BEARER) for API key, if needed
# swagger_client.configuration.api_key_prefix['api_key'] = 'BEARER'
# Configure OAuth2 access token for authorization: petstore_auth
swagger_client.configuration.access_token = 'YOUR_ACCESS_TOKEN'

# create an instance of the API class
api_instance = swagger_client.PetApi()
pet_id = 789 # int | ID of pet that needs to be fetched

try: 
    # Fake endpoint to test inline arbitrary object return by 'Find pet by ID'
    api_response = api_instance.get_pet_by_id_in_object(pet_id);
    pprint(api_response)
except ApiException as e:
    print "Exception when calling PetApi->get_pet_by_id_in_object: %s\n" % e
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet_id** | **int**| ID of pet that needs to be fetched | 

### Return type

[**InlineResponse200**](InlineResponse200.md)

### Authorization

[api_key](../README.md#api_key), [petstore_auth](../README.md#petstore_auth)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **pet_pet_idtesting_byte_arraytrue_get**
> str pet_pet_idtesting_byte_arraytrue_get(pet_id)

Fake endpoint to test byte array return by 'Find pet by ID'

Returns a pet when ID < 10.  ID > 10 or nonintegers will simulate API error conditions

### Example 
```python
import swagger_client
from swagger_client.rest import ApiException
from pprint import pprint
import time


# Configure API key authorization: api_key
swagger_client.configuration.api_key['api_key'] = 'YOUR_API_KEY';
# Uncomment below to setup prefix (e.g. BEARER) for API key, if needed
# swagger_client.configuration.api_key_prefix['api_key'] = 'BEARER'
# Configure OAuth2 access token for authorization: petstore_auth
swagger_client.configuration.access_token = 'YOUR_ACCESS_TOKEN'

# create an instance of the API class
api_instance = swagger_client.PetApi()
pet_id = 789 # int | ID of pet that needs to be fetched

try: 
    # Fake endpoint to test byte array return by 'Find pet by ID'
    api_response = api_instance.pet_pet_idtesting_byte_arraytrue_get(pet_id);
    pprint(api_response)
except ApiException as e:
    print "Exception when calling PetApi->pet_pet_idtesting_byte_arraytrue_get: %s\n" % e
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet_id** | **int**| ID of pet that needs to be fetched | 

### Return type

**str**

### Authorization

[api_key](../README.md#api_key), [petstore_auth](../README.md#petstore_auth)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **update_pet**
> update_pet(body=body)

Update an existing pet



### Example 
```python
import swagger_client
from swagger_client.rest import ApiException
from pprint import pprint
import time


# Configure OAuth2 access token for authorization: petstore_auth
swagger_client.configuration.access_token = 'YOUR_ACCESS_TOKEN'

# create an instance of the API class
api_instance = swagger_client.PetApi()
body = swagger_client.Pet() # Pet | Pet object that needs to be added to the store (optional)

try: 
    # Update an existing pet
    api_instance.update_pet(body=body);
except ApiException as e:
    print "Exception when calling PetApi->update_pet: %s\n" % e
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Pet**](Pet.md)| Pet object that needs to be added to the store | [optional] 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP reuqest headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **update_pet_with_form**
> update_pet_with_form(pet_id, name=name, status=status)

Updates a pet in the store with form data



### Example 
```python
import swagger_client
from swagger_client.rest import ApiException
from pprint import pprint
import time


# Configure OAuth2 access token for authorization: petstore_auth
swagger_client.configuration.access_token = 'YOUR_ACCESS_TOKEN'

# create an instance of the API class
api_instance = swagger_client.PetApi()
pet_id = 'pet_id_example' # str | ID of pet that needs to be updated
name = 'name_example' # str | Updated name of the pet (optional)
status = 'status_example' # str | Updated status of the pet (optional)

try: 
    # Updates a pet in the store with form data
    api_instance.update_pet_with_form(pet_id, name=name, status=status);
except ApiException as e:
    print "Exception when calling PetApi->update_pet_with_form: %s\n" % e
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet_id** | **str**| ID of pet that needs to be updated | 
 **name** | **str**| Updated name of the pet | [optional] 
 **status** | **str**| Updated status of the pet | [optional] 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP reuqest headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **upload_file**
> upload_file(pet_id, additional_metadata=additional_metadata, file=file)

uploads an image



### Example 
```python
import swagger_client
from swagger_client.rest import ApiException
from pprint import pprint
import time


# Configure OAuth2 access token for authorization: petstore_auth
swagger_client.configuration.access_token = 'YOUR_ACCESS_TOKEN'

# create an instance of the API class
api_instance = swagger_client.PetApi()
pet_id = 789 # int | ID of pet to update
additional_metadata = 'additional_metadata_example' # str | Additional data to pass to server (optional)
file = '/path/to/file.txt' # file | file to upload (optional)

try: 
    # uploads an image
    api_instance.upload_file(pet_id, additional_metadata=additional_metadata, file=file);
except ApiException as e:
    print "Exception when calling PetApi->upload_file: %s\n" % e
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet_id** | **int**| ID of pet to update | 
 **additional_metadata** | **str**| Additional data to pass to server | [optional] 
 **file** | **file**| file to upload | [optional] 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP reuqest headers

 - **Content-Type**: multipart/form-data
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

