# ::PetApi

## Load the API package
```perl
use ::Object::PetApi;
```

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**addPet**](PetApi.md#addPet) | **POST** /pet | Add a new pet to the store
[**addPetUsingByteArray**](PetApi.md#addPetUsingByteArray) | **POST** /pet?testing_byte_array=true | Fake endpoint to test byte array in body parameter for adding a new pet to the store
[**deletePet**](PetApi.md#deletePet) | **DELETE** /pet/{petId} | Deletes a pet
[**findPetsByStatus**](PetApi.md#findPetsByStatus) | **GET** /pet/findByStatus | Finds Pets by status
[**findPetsByTags**](PetApi.md#findPetsByTags) | **GET** /pet/findByTags | Finds Pets by tags
[**getPetById**](PetApi.md#getPetById) | **GET** /pet/{petId} | Find pet by ID
[**getPetByIdInObject**](PetApi.md#getPetByIdInObject) | **GET** /pet/{petId}?response=inline_arbitrary_object | Fake endpoint to test inline arbitrary object return by &#39;Find pet by ID&#39;
[**petPetIdtestingByteArraytrueGet**](PetApi.md#petPetIdtestingByteArraytrueGet) | **GET** /pet/{petId}?testing_byte_array=true | Fake endpoint to test byte array return by &#39;Find pet by ID&#39;
[**updatePet**](PetApi.md#updatePet) | **PUT** /pet | Update an existing pet
[**updatePetWithForm**](PetApi.md#updatePetWithForm) | **POST** /pet/{petId} | Updates a pet in the store with form data
[**uploadFile**](PetApi.md#uploadFile) | **POST** /pet/{petId}/uploadImage | uploads an image


# **addPet**
> addPet(body => $body)

Add a new pet to the store



### Example 
```perl
use Data::Dumper;

# Configure OAuth2 access token for authorization: petstore_auth
::Configuration::access_token = 'YOUR_ACCESS_TOKEN';

my $api = ::PetApi->new();
my $body = ::Object::\Swagger\Client\Model\Pet->new(); # [\Swagger\Client\Model\Pet] Pet object that needs to be added to the store

eval { 
    $api->addPet(body => $body);
};
if ($@) {
    warn "Exception when calling PetApi->addPet: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**\Swagger\Client\Model\Pet**](\Swagger\Client\Model\Pet.md)| Pet object that needs to be added to the store | [optional] 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP reuqest headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **addPetUsingByteArray**
> addPetUsingByteArray(body => $body)

Fake endpoint to test byte array in body parameter for adding a new pet to the store



### Example 
```perl
use Data::Dumper;

# Configure OAuth2 access token for authorization: petstore_auth
::Configuration::access_token = 'YOUR_ACCESS_TOKEN';

my $api = ::PetApi->new();
my $body = ::Object::string->new(); # [string] Pet object in the form of byte array

eval { 
    $api->addPetUsingByteArray(body => $body);
};
if ($@) {
    warn "Exception when calling PetApi->addPetUsingByteArray: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **string**| Pet object in the form of byte array | [optional] 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP reuqest headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **deletePet**
> deletePet(pet_id => $pet_id, api_key => $api_key)

Deletes a pet



### Example 
```perl
use Data::Dumper;

# Configure OAuth2 access token for authorization: petstore_auth
::Configuration::access_token = 'YOUR_ACCESS_TOKEN';

my $api = ::PetApi->new();
my $pet_id = 789; # [int] Pet id to delete
my $api_key = api_key_example; # [string] 

eval { 
    $api->deletePet(pet_id => $pet_id, api_key => $api_key);
};
if ($@) {
    warn "Exception when calling PetApi->deletePet: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet_id** | **int**| Pet id to delete | 
 **api_key** | **string**|  | [optional] 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **findPetsByStatus**
> \Swagger\Client\Model\Pet[] findPetsByStatus(status => $status)

Finds Pets by status

Multiple status values can be provided with comma separated strings

### Example 
```perl
use Data::Dumper;

# Configure OAuth2 access token for authorization: petstore_auth
::Configuration::access_token = 'YOUR_ACCESS_TOKEN';

my $api = ::PetApi->new();
my $status = (array(available)); # [string[]] Status values that need to be considered for query

eval { 
    my $result = $api->findPetsByStatus(status => $status);
    print Dumper($result);
};
if ($@) {
    warn "Exception when calling PetApi->findPetsByStatus: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **status** | [**string[]**](string.md)| Status values that need to be considered for query | [optional] [default to available]

### Return type

[**\Swagger\Client\Model\Pet[]**](Pet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **findPetsByTags**
> \Swagger\Client\Model\Pet[] findPetsByTags(tags => $tags)

Finds Pets by tags

Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.

### Example 
```perl
use Data::Dumper;

# Configure OAuth2 access token for authorization: petstore_auth
::Configuration::access_token = 'YOUR_ACCESS_TOKEN';

my $api = ::PetApi->new();
my $tags = (nil); # [string[]] Tags to filter by

eval { 
    my $result = $api->findPetsByTags(tags => $tags);
    print Dumper($result);
};
if ($@) {
    warn "Exception when calling PetApi->findPetsByTags: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **tags** | [**string[]**](string.md)| Tags to filter by | [optional] 

### Return type

[**\Swagger\Client\Model\Pet[]**](Pet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **getPetById**
> \Swagger\Client\Model\Pet getPetById(pet_id => $pet_id)

Find pet by ID

Returns a pet when ID < 10.  ID > 10 or nonintegers will simulate API error conditions

### Example 
```perl
use Data::Dumper;

# Configure API key authorization: api_key
::Configuration::api_key->{'api_key'} = 'YOUR_API_KEY';
# uncomment below to setup prefix (e.g. BEARER) for API key, if needed
#::Configuration::api_key_prefix->{'api_key'} = "BEARER";
# Configure OAuth2 access token for authorization: petstore_auth
::Configuration::access_token = 'YOUR_ACCESS_TOKEN';

my $api = ::PetApi->new();
my $pet_id = 789; # [int] ID of pet that needs to be fetched

eval { 
    my $result = $api->getPetById(pet_id => $pet_id);
    print Dumper($result);
};
if ($@) {
    warn "Exception when calling PetApi->getPetById: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet_id** | **int**| ID of pet that needs to be fetched | 

### Return type

[**\Swagger\Client\Model\Pet**](Pet.md)

### Authorization

[api_key](../README.md#api_key), [petstore_auth](../README.md#petstore_auth)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **getPetByIdInObject**
> \Swagger\Client\Model\InlineResponse200 getPetByIdInObject(pet_id => $pet_id)

Fake endpoint to test inline arbitrary object return by 'Find pet by ID'

Returns a pet when ID < 10.  ID > 10 or nonintegers will simulate API error conditions

### Example 
```perl
use Data::Dumper;

# Configure API key authorization: api_key
::Configuration::api_key->{'api_key'} = 'YOUR_API_KEY';
# uncomment below to setup prefix (e.g. BEARER) for API key, if needed
#::Configuration::api_key_prefix->{'api_key'} = "BEARER";
# Configure OAuth2 access token for authorization: petstore_auth
::Configuration::access_token = 'YOUR_ACCESS_TOKEN';

my $api = ::PetApi->new();
my $pet_id = 789; # [int] ID of pet that needs to be fetched

eval { 
    my $result = $api->getPetByIdInObject(pet_id => $pet_id);
    print Dumper($result);
};
if ($@) {
    warn "Exception when calling PetApi->getPetByIdInObject: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet_id** | **int**| ID of pet that needs to be fetched | 

### Return type

[**\Swagger\Client\Model\InlineResponse200**](InlineResponse200.md)

### Authorization

[api_key](../README.md#api_key), [petstore_auth](../README.md#petstore_auth)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **petPetIdtestingByteArraytrueGet**
> string petPetIdtestingByteArraytrueGet(pet_id => $pet_id)

Fake endpoint to test byte array return by 'Find pet by ID'

Returns a pet when ID < 10.  ID > 10 or nonintegers will simulate API error conditions

### Example 
```perl
use Data::Dumper;

# Configure API key authorization: api_key
::Configuration::api_key->{'api_key'} = 'YOUR_API_KEY';
# uncomment below to setup prefix (e.g. BEARER) for API key, if needed
#::Configuration::api_key_prefix->{'api_key'} = "BEARER";
# Configure OAuth2 access token for authorization: petstore_auth
::Configuration::access_token = 'YOUR_ACCESS_TOKEN';

my $api = ::PetApi->new();
my $pet_id = 789; # [int] ID of pet that needs to be fetched

eval { 
    my $result = $api->petPetIdtestingByteArraytrueGet(pet_id => $pet_id);
    print Dumper($result);
};
if ($@) {
    warn "Exception when calling PetApi->petPetIdtestingByteArraytrueGet: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet_id** | **int**| ID of pet that needs to be fetched | 

### Return type

**string**

### Authorization

[api_key](../README.md#api_key), [petstore_auth](../README.md#petstore_auth)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **updatePet**
> updatePet(body => $body)

Update an existing pet



### Example 
```perl
use Data::Dumper;

# Configure OAuth2 access token for authorization: petstore_auth
::Configuration::access_token = 'YOUR_ACCESS_TOKEN';

my $api = ::PetApi->new();
my $body = ::Object::\Swagger\Client\Model\Pet->new(); # [\Swagger\Client\Model\Pet] Pet object that needs to be added to the store

eval { 
    $api->updatePet(body => $body);
};
if ($@) {
    warn "Exception when calling PetApi->updatePet: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**\Swagger\Client\Model\Pet**](\Swagger\Client\Model\Pet.md)| Pet object that needs to be added to the store | [optional] 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP reuqest headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **updatePetWithForm**
> updatePetWithForm(pet_id => $pet_id, name => $name, status => $status)

Updates a pet in the store with form data



### Example 
```perl
use Data::Dumper;

# Configure OAuth2 access token for authorization: petstore_auth
::Configuration::access_token = 'YOUR_ACCESS_TOKEN';

my $api = ::PetApi->new();
my $pet_id = pet_id_example; # [string] ID of pet that needs to be updated
my $name = name_example; # [string] Updated name of the pet
my $status = status_example; # [string] Updated status of the pet

eval { 
    $api->updatePetWithForm(pet_id => $pet_id, name => $name, status => $status);
};
if ($@) {
    warn "Exception when calling PetApi->updatePetWithForm: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet_id** | **string**| ID of pet that needs to be updated | 
 **name** | **string**| Updated name of the pet | [optional] 
 **status** | **string**| Updated status of the pet | [optional] 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP reuqest headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **uploadFile**
> uploadFile(pet_id => $pet_id, additional_metadata => $additional_metadata, file => $file)

uploads an image



### Example 
```perl
use Data::Dumper;

# Configure OAuth2 access token for authorization: petstore_auth
::Configuration::access_token = 'YOUR_ACCESS_TOKEN';

my $api = ::PetApi->new();
my $pet_id = 789; # [int] ID of pet to update
my $additional_metadata = additional_metadata_example; # [string] Additional data to pass to server
my $file = new Swagger\Client\\SplFileObject(); # [\SplFileObject] file to upload

eval { 
    $api->uploadFile(pet_id => $pet_id, additional_metadata => $additional_metadata, file => $file);
};
if ($@) {
    warn "Exception when calling PetApi->uploadFile: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet_id** | **int**| ID of pet to update | 
 **additional_metadata** | **string**| Additional data to pass to server | [optional] 
 **file** | **\SplFileObject**| file to upload | [optional] 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP reuqest headers

 - **Content-Type**: multipart/form-data
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

