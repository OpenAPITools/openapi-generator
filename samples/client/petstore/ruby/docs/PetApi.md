# Petstore::PetApi

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
> add_pet(opts)

Add a new pet to the store



### Example
```ruby
# load the gem
require 'petstore'
# setup authorization 
Petstore.configure do |config|
  # Configure OAuth2 access token for authorization: petstore_auth
  config.access_token = 'YOUR ACCESS TOKEN'
end

api_instance = Petstore::PetApi.new

opts = { 
  body: Petstore::Pet.new # Pet | Pet object that needs to be added to the store
}

begin
  #Add a new pet to the store
  api_instance.add_pet(opts)
rescue Petstore::ApiError => e
  puts "Exception when calling PetApi->add_pet: #{e}"
end
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Pet**](Pet.md)| Pet object that needs to be added to the store | [optional] 

### Return type

nil (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP reuqest headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: application/json, application/xml



# **add_pet_using_byte_array**
> add_pet_using_byte_array(opts)

Fake endpoint to test byte array in body parameter for adding a new pet to the store



### Example
```ruby
# load the gem
require 'petstore'
# setup authorization 
Petstore.configure do |config|
  # Configure OAuth2 access token for authorization: petstore_auth
  config.access_token = 'YOUR ACCESS TOKEN'
end

api_instance = Petstore::PetApi.new

opts = { 
  body: "B" # String | Pet object in the form of byte array
}

begin
  #Fake endpoint to test byte array in body parameter for adding a new pet to the store
  api_instance.add_pet_using_byte_array(opts)
rescue Petstore::ApiError => e
  puts "Exception when calling PetApi->add_pet_using_byte_array: #{e}"
end
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **String**| Pet object in the form of byte array | [optional] 

### Return type

nil (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP reuqest headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: application/json, application/xml



# **delete_pet**
> delete_pet(pet_id, opts)

Deletes a pet



### Example
```ruby
# load the gem
require 'petstore'
# setup authorization 
Petstore.configure do |config|
  # Configure OAuth2 access token for authorization: petstore_auth
  config.access_token = 'YOUR ACCESS TOKEN'
end

api_instance = Petstore::PetApi.new

pet_id = 789 # Integer | Pet id to delete

opts = { 
  api_key: "api_key_example" # String | 
}

begin
  #Deletes a pet
  api_instance.delete_pet(pet_id, opts)
rescue Petstore::ApiError => e
  puts "Exception when calling PetApi->delete_pet: #{e}"
end
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet_id** | **Integer**| Pet id to delete | 
 **api_key** | **String**|  | [optional] 

### Return type

nil (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml



# **find_pets_by_status**
> Array&lt;Pet&gt; find_pets_by_status(opts)

Finds Pets by status

Multiple status values can be provided with comma separated strings

### Example
```ruby
# load the gem
require 'petstore'
# setup authorization 
Petstore.configure do |config|
  # Configure OAuth2 access token for authorization: petstore_auth
  config.access_token = 'YOUR ACCESS TOKEN'
end

api_instance = Petstore::PetApi.new

opts = { 
  status: ["available"] # Array<String> | Status values that need to be considered for query
}

begin
  #Finds Pets by status
  result = api_instance.find_pets_by_status(opts)
  p result
rescue Petstore::ApiError => e
  puts "Exception when calling PetApi->find_pets_by_status: #{e}"
end
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **status** | [**Array&lt;String&gt;**](String.md)| Status values that need to be considered for query | [optional] [default to available]

### Return type

[**Array&lt;Pet&gt;**](Pet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml



# **find_pets_by_tags**
> Array&lt;Pet&gt; find_pets_by_tags(opts)

Finds Pets by tags

Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.

### Example
```ruby
# load the gem
require 'petstore'
# setup authorization 
Petstore.configure do |config|
  # Configure OAuth2 access token for authorization: petstore_auth
  config.access_token = 'YOUR ACCESS TOKEN'
end

api_instance = Petstore::PetApi.new

opts = { 
  tags: ["tags_example"] # Array<String> | Tags to filter by
}

begin
  #Finds Pets by tags
  result = api_instance.find_pets_by_tags(opts)
  p result
rescue Petstore::ApiError => e
  puts "Exception when calling PetApi->find_pets_by_tags: #{e}"
end
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **tags** | [**Array&lt;String&gt;**](String.md)| Tags to filter by | [optional] 

### Return type

[**Array&lt;Pet&gt;**](Pet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml



# **get_pet_by_id**
> Pet get_pet_by_id(pet_id)

Find pet by ID

Returns a pet when ID < 10.  ID > 10 or nonintegers will simulate API error conditions

### Example
```ruby
# load the gem
require 'petstore'
# setup authorization 
Petstore.configure do |config|
  # Configure API key authorization: api_key
  config.api_key['api_key'] = 'YOUR API KEY'
  # Uncomment the following line to set a prefix for the API key, e.g. 'BEARER' (defaults to nil)
  #config.api_key_prefix['api_key'] = 'BEARER'

  # Configure OAuth2 access token for authorization: petstore_auth
  config.access_token = 'YOUR ACCESS TOKEN'
end

api_instance = Petstore::PetApi.new

pet_id = 789 # Integer | ID of pet that needs to be fetched


begin
  #Find pet by ID
  result = api_instance.get_pet_by_id(pet_id)
  p result
rescue Petstore::ApiError => e
  puts "Exception when calling PetApi->get_pet_by_id: #{e}"
end
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet_id** | **Integer**| ID of pet that needs to be fetched | 

### Return type

[**Pet**](Pet.md)

### Authorization

[api_key](../README.md#api_key), [petstore_auth](../README.md#petstore_auth)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml



# **get_pet_by_id_in_object**
> InlineResponse200 get_pet_by_id_in_object(pet_id)

Fake endpoint to test inline arbitrary object return by 'Find pet by ID'

Returns a pet when ID < 10.  ID > 10 or nonintegers will simulate API error conditions

### Example
```ruby
# load the gem
require 'petstore'
# setup authorization 
Petstore.configure do |config|
  # Configure API key authorization: api_key
  config.api_key['api_key'] = 'YOUR API KEY'
  # Uncomment the following line to set a prefix for the API key, e.g. 'BEARER' (defaults to nil)
  #config.api_key_prefix['api_key'] = 'BEARER'

  # Configure OAuth2 access token for authorization: petstore_auth
  config.access_token = 'YOUR ACCESS TOKEN'
end

api_instance = Petstore::PetApi.new

pet_id = 789 # Integer | ID of pet that needs to be fetched


begin
  #Fake endpoint to test inline arbitrary object return by 'Find pet by ID'
  result = api_instance.get_pet_by_id_in_object(pet_id)
  p result
rescue Petstore::ApiError => e
  puts "Exception when calling PetApi->get_pet_by_id_in_object: #{e}"
end
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet_id** | **Integer**| ID of pet that needs to be fetched | 

### Return type

[**InlineResponse200**](InlineResponse200.md)

### Authorization

[api_key](../README.md#api_key), [petstore_auth](../README.md#petstore_auth)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml



# **pet_pet_idtesting_byte_arraytrue_get**
> String pet_pet_idtesting_byte_arraytrue_get(pet_id)

Fake endpoint to test byte array return by 'Find pet by ID'

Returns a pet when ID < 10.  ID > 10 or nonintegers will simulate API error conditions

### Example
```ruby
# load the gem
require 'petstore'
# setup authorization 
Petstore.configure do |config|
  # Configure API key authorization: api_key
  config.api_key['api_key'] = 'YOUR API KEY'
  # Uncomment the following line to set a prefix for the API key, e.g. 'BEARER' (defaults to nil)
  #config.api_key_prefix['api_key'] = 'BEARER'

  # Configure OAuth2 access token for authorization: petstore_auth
  config.access_token = 'YOUR ACCESS TOKEN'
end

api_instance = Petstore::PetApi.new

pet_id = 789 # Integer | ID of pet that needs to be fetched


begin
  #Fake endpoint to test byte array return by 'Find pet by ID'
  result = api_instance.pet_pet_idtesting_byte_arraytrue_get(pet_id)
  p result
rescue Petstore::ApiError => e
  puts "Exception when calling PetApi->pet_pet_idtesting_byte_arraytrue_get: #{e}"
end
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet_id** | **Integer**| ID of pet that needs to be fetched | 

### Return type

**String**

### Authorization

[api_key](../README.md#api_key), [petstore_auth](../README.md#petstore_auth)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml



# **update_pet**
> update_pet(opts)

Update an existing pet



### Example
```ruby
# load the gem
require 'petstore'
# setup authorization 
Petstore.configure do |config|
  # Configure OAuth2 access token for authorization: petstore_auth
  config.access_token = 'YOUR ACCESS TOKEN'
end

api_instance = Petstore::PetApi.new

opts = { 
  body: Petstore::Pet.new # Pet | Pet object that needs to be added to the store
}

begin
  #Update an existing pet
  api_instance.update_pet(opts)
rescue Petstore::ApiError => e
  puts "Exception when calling PetApi->update_pet: #{e}"
end
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Pet**](Pet.md)| Pet object that needs to be added to the store | [optional] 

### Return type

nil (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP reuqest headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: application/json, application/xml



# **update_pet_with_form**
> update_pet_with_form(pet_id, opts)

Updates a pet in the store with form data



### Example
```ruby
# load the gem
require 'petstore'
# setup authorization 
Petstore.configure do |config|
  # Configure OAuth2 access token for authorization: petstore_auth
  config.access_token = 'YOUR ACCESS TOKEN'
end

api_instance = Petstore::PetApi.new

pet_id = "pet_id_example" # String | ID of pet that needs to be updated

opts = { 
  name: "name_example", # String | Updated name of the pet
  status: "status_example" # String | Updated status of the pet
}

begin
  #Updates a pet in the store with form data
  api_instance.update_pet_with_form(pet_id, opts)
rescue Petstore::ApiError => e
  puts "Exception when calling PetApi->update_pet_with_form: #{e}"
end
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet_id** | **String**| ID of pet that needs to be updated | 
 **name** | **String**| Updated name of the pet | [optional] 
 **status** | **String**| Updated status of the pet | [optional] 

### Return type

nil (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP reuqest headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: application/json, application/xml



# **upload_file**
> upload_file(pet_id, opts)

uploads an image



### Example
```ruby
# load the gem
require 'petstore'
# setup authorization 
Petstore.configure do |config|
  # Configure OAuth2 access token for authorization: petstore_auth
  config.access_token = 'YOUR ACCESS TOKEN'
end

api_instance = Petstore::PetApi.new

pet_id = 789 # Integer | ID of pet to update

opts = { 
  additional_metadata: "additional_metadata_example", # String | Additional data to pass to server
  file: File.new("/path/to/file.txt") # File | file to upload
}

begin
  #uploads an image
  api_instance.upload_file(pet_id, opts)
rescue Petstore::ApiError => e
  puts "Exception when calling PetApi->upload_file: #{e}"
end
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet_id** | **Integer**| ID of pet to update | 
 **additional_metadata** | **String**| Additional data to pass to server | [optional] 
 **file** | **File**| file to upload | [optional] 

### Return type

nil (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP reuqest headers

 - **Content-Type**: multipart/form-data
 - **Accept**: application/json, application/xml



