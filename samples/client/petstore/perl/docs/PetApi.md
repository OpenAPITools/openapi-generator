# WWW::SwaggerClient::PetApi

## Load the API package
```perl
use WWW::SwaggerClient::Object::PetApi;
```

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


# **add_pet**
> add_pet(body => $body)

Add a new pet to the store



### Example 
```perl
use Data::Dumper;
use WWW::SwaggerClient::PetApi;
my $api_instance = WWW::SwaggerClient::PetApi->new(

    # Configure OAuth2 access token for authorization: petstore_auth
    access_token => 'YOUR_ACCESS_TOKEN',
);

my $body = WWW::SwaggerClient::Object::Pet->new(); # Pet | Pet object that needs to be added to the store

eval { 
    $api_instance->add_pet(body => $body);
};
if ($@) {
    warn "Exception when calling PetApi->add_pet: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Pet**](Pet.md)| Pet object that needs to be added to the store | 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **delete_pet**
> delete_pet(pet_id => $pet_id, api_key => $api_key)

Deletes a pet



### Example 
```perl
use Data::Dumper;
use WWW::SwaggerClient::PetApi;
my $api_instance = WWW::SwaggerClient::PetApi->new(

    # Configure OAuth2 access token for authorization: petstore_auth
    access_token => 'YOUR_ACCESS_TOKEN',
);

my $pet_id = 789; # int | Pet id to delete
my $api_key = 'api_key_example'; # string | 

eval { 
    $api_instance->delete_pet(pet_id => $pet_id, api_key => $api_key);
};
if ($@) {
    warn "Exception when calling PetApi->delete_pet: $@\n";
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

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **find_pets_by_status**
> ARRAY[Pet] find_pets_by_status(status => $status)

Finds Pets by status

Multiple status values can be provided with comma separated strings

### Example 
```perl
use Data::Dumper;
use WWW::SwaggerClient::PetApi;
my $api_instance = WWW::SwaggerClient::PetApi->new(

    # Configure OAuth2 access token for authorization: petstore_auth
    access_token => 'YOUR_ACCESS_TOKEN',
);

my $status = []; # ARRAY[string] | Status values that need to be considered for filter

eval { 
    my $result = $api_instance->find_pets_by_status(status => $status);
    print Dumper($result);
};
if ($@) {
    warn "Exception when calling PetApi->find_pets_by_status: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **status** | [**ARRAY[string]**](string.md)| Status values that need to be considered for filter | 

### Return type

[**ARRAY[Pet]**](Pet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **find_pets_by_tags**
> ARRAY[Pet] find_pets_by_tags(tags => $tags)

Finds Pets by tags

Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.

### Example 
```perl
use Data::Dumper;
use WWW::SwaggerClient::PetApi;
my $api_instance = WWW::SwaggerClient::PetApi->new(

    # Configure OAuth2 access token for authorization: petstore_auth
    access_token => 'YOUR_ACCESS_TOKEN',
);

my $tags = []; # ARRAY[string] | Tags to filter by

eval { 
    my $result = $api_instance->find_pets_by_tags(tags => $tags);
    print Dumper($result);
};
if ($@) {
    warn "Exception when calling PetApi->find_pets_by_tags: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **tags** | [**ARRAY[string]**](string.md)| Tags to filter by | 

### Return type

[**ARRAY[Pet]**](Pet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **get_pet_by_id**
> Pet get_pet_by_id(pet_id => $pet_id)

Find pet by ID

Returns a single pet

### Example 
```perl
use Data::Dumper;
use WWW::SwaggerClient::PetApi;
my $api_instance = WWW::SwaggerClient::PetApi->new(

    # Configure API key authorization: api_key
    api_key => {'api_key' => 'YOUR_API_KEY'},
    # uncomment below to setup prefix (e.g. Bearer) for API key, if needed
    #api_key_prefix => {'api_key' => 'Bearer'},
);

my $pet_id = 789; # int | ID of pet to return

eval { 
    my $result = $api_instance->get_pet_by_id(pet_id => $pet_id);
    print Dumper($result);
};
if ($@) {
    warn "Exception when calling PetApi->get_pet_by_id: $@\n";
}
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
> update_pet(body => $body)

Update an existing pet



### Example 
```perl
use Data::Dumper;
use WWW::SwaggerClient::PetApi;
my $api_instance = WWW::SwaggerClient::PetApi->new(

    # Configure OAuth2 access token for authorization: petstore_auth
    access_token => 'YOUR_ACCESS_TOKEN',
);

my $body = WWW::SwaggerClient::Object::Pet->new(); # Pet | Pet object that needs to be added to the store

eval { 
    $api_instance->update_pet(body => $body);
};
if ($@) {
    warn "Exception when calling PetApi->update_pet: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Pet**](Pet.md)| Pet object that needs to be added to the store | 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **update_pet_with_form**
> update_pet_with_form(pet_id => $pet_id, name => $name, status => $status)

Updates a pet in the store with form data



### Example 
```perl
use Data::Dumper;
use WWW::SwaggerClient::PetApi;
my $api_instance = WWW::SwaggerClient::PetApi->new(

    # Configure OAuth2 access token for authorization: petstore_auth
    access_token => 'YOUR_ACCESS_TOKEN',
);

my $pet_id = 789; # int | ID of pet that needs to be updated
my $name = 'name_example'; # string | Updated name of the pet
my $status = 'status_example'; # string | Updated status of the pet

eval { 
    $api_instance->update_pet_with_form(pet_id => $pet_id, name => $name, status => $status);
};
if ($@) {
    warn "Exception when calling PetApi->update_pet_with_form: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet_id** | **int**| ID of pet that needs to be updated | 
 **name** | **string**| Updated name of the pet | [optional] 
 **status** | **string**| Updated status of the pet | [optional] 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **upload_file**
> ApiResponse upload_file(pet_id => $pet_id, additional_metadata => $additional_metadata, file => $file)

uploads an image



### Example 
```perl
use Data::Dumper;
use WWW::SwaggerClient::PetApi;
my $api_instance = WWW::SwaggerClient::PetApi->new(

    # Configure OAuth2 access token for authorization: petstore_auth
    access_token => 'YOUR_ACCESS_TOKEN',
);

my $pet_id = 789; # int | ID of pet to update
my $additional_metadata = 'additional_metadata_example'; # string | Additional data to pass to server
my $file = '/path/to/file.txt'; # File | file to upload

eval { 
    my $result = $api_instance->upload_file(pet_id => $pet_id, additional_metadata => $additional_metadata, file => $file);
    print Dumper($result);
};
if ($@) {
    warn "Exception when calling PetApi->upload_file: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet_id** | **int**| ID of pet to update | 
 **additional_metadata** | **string**| Additional data to pass to server | [optional] 
 **file** | **File**| file to upload | [optional] 

### Return type

[**ApiResponse**](ApiResponse.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

