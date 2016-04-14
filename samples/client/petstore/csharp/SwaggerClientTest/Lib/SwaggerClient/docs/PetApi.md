# IO.Swagger.Api.PetApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**AddPet**](PetApi.md#AddPet) | **POST** /pet | Add a new pet to the store
[**AddPetUsingByteArray**](PetApi.md#AddPetUsingByteArray) | **POST** /pet?testing_byte_array&#x3D;true | Fake endpoint to test byte array in body parameter for adding a new pet to the store
[**DeletePet**](PetApi.md#DeletePet) | **DELETE** /pet/{petId} | Deletes a pet
[**FindPetsByStatus**](PetApi.md#FindPetsByStatus) | **GET** /pet/findByStatus | Finds Pets by status
[**FindPetsByTags**](PetApi.md#FindPetsByTags) | **GET** /pet/findByTags | Finds Pets by tags
[**GetPetById**](PetApi.md#GetPetById) | **GET** /pet/{petId} | Find pet by ID
[**GetPetByIdInObject**](PetApi.md#GetPetByIdInObject) | **GET** /pet/{petId}?response&#x3D;inline_arbitrary_object | Fake endpoint to test inline arbitrary object return by &#39;Find pet by ID&#39;
[**PetPetIdtestingByteArraytrueGet**](PetApi.md#PetPetIdtestingByteArraytrueGet) | **GET** /pet/{petId}?testing_byte_array&#x3D;true | Fake endpoint to test byte array return by &#39;Find pet by ID&#39;
[**UpdatePet**](PetApi.md#UpdatePet) | **PUT** /pet | Update an existing pet
[**UpdatePetWithForm**](PetApi.md#UpdatePetWithForm) | **POST** /pet/{petId} | Updates a pet in the store with form data
[**UploadFile**](PetApi.md#UploadFile) | **POST** /pet/{petId}/uploadImage | uploads an image


# **AddPet**
> AddPet(body)

Add a new pet to the store



### Example 
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Module;

namespace Example
{
    public class AddPetExample
    {
        public void main(){
            
            // Configure OAuth2 access token for authorization: petstore_auth
            Configuration.Default.AccessToken = 'YOUR_ACCESS_TOKEN';

            var apiInstance = new PetApi();
            var body = new Pet(); // Pet | Pet object that needs to be added to the store

            try {
                apiInstance.AddPet(body);
            } catch (Exception e) {
                Debug.Print("Exception when calling PetApi.AddPet: " + e.Message );
            }
        }
    }
}
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

# **AddPetUsingByteArray**
> AddPetUsingByteArray(body)

Fake endpoint to test byte array in body parameter for adding a new pet to the store



### Example 
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Module;

namespace Example
{
    public class AddPetUsingByteArrayExample
    {
        public void main(){
            
            // Configure OAuth2 access token for authorization: petstore_auth
            Configuration.Default.AccessToken = 'YOUR_ACCESS_TOKEN';

            var apiInstance = new PetApi();
            var body = BINARY_DATA_HERE;  // byte[] | Pet object in the form of byte array

            try {
                apiInstance.AddPetUsingByteArray(body);
            } catch (Exception e) {
                Debug.Print("Exception when calling PetApi.AddPetUsingByteArray: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **byte[]**| Pet object in the form of byte array | [optional] 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP reuqest headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **DeletePet**
> DeletePet(petId, apiKey)

Deletes a pet



### Example 
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Module;

namespace Example
{
    public class DeletePetExample
    {
        public void main(){
            
            // Configure OAuth2 access token for authorization: petstore_auth
            Configuration.Default.AccessToken = 'YOUR_ACCESS_TOKEN';

            var apiInstance = new PetApi();
            var petId = 789;  // long? | Pet id to delete
            var apiKey = apiKey_example;  // string | 

            try {
                apiInstance.DeletePet(petId, apiKey);
            } catch (Exception e) {
                Debug.Print("Exception when calling PetApi.DeletePet: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **long?**| Pet id to delete | 
 **apiKey** | **string**|  | [optional] 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **FindPetsByStatus**
> List<Pet> FindPetsByStatus(status)

Finds Pets by status

Multiple status values can be provided with comma separated strings

### Example 
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Module;

namespace Example
{
    public class FindPetsByStatusExample
    {
        public void main(){
            
            // Configure OAuth2 access token for authorization: petstore_auth
            Configuration.Default.AccessToken = 'YOUR_ACCESS_TOKEN';

            var apiInstance = new PetApi();
            var status = new List<string>(); // List<string> | Status values that need to be considered for query

            try {
                List&lt;Pet&gt; result = apiInstance.FindPetsByStatus(status);
                Debug.WriteLine(result);
            } catch (Exception e) {
                Debug.Print("Exception when calling PetApi.FindPetsByStatus: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **status** | [**List&lt;string&gt;**](string.md)| Status values that need to be considered for query | [optional] [default to available]

### Return type

[**List<Pet>**](Pet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **FindPetsByTags**
> List<Pet> FindPetsByTags(tags)

Finds Pets by tags

Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.

### Example 
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Module;

namespace Example
{
    public class FindPetsByTagsExample
    {
        public void main(){
            
            // Configure OAuth2 access token for authorization: petstore_auth
            Configuration.Default.AccessToken = 'YOUR_ACCESS_TOKEN';

            var apiInstance = new PetApi();
            var tags = new List<string>(); // List<string> | Tags to filter by

            try {
                List&lt;Pet&gt; result = apiInstance.FindPetsByTags(tags);
                Debug.WriteLine(result);
            } catch (Exception e) {
                Debug.Print("Exception when calling PetApi.FindPetsByTags: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **tags** | [**List&lt;string&gt;**](string.md)| Tags to filter by | [optional] 

### Return type

[**List<Pet>**](Pet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **GetPetById**
> Pet GetPetById(petId)

Find pet by ID

Returns a pet when ID < 10.  ID > 10 or nonintegers will simulate API error conditions

### Example 
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Module;

namespace Example
{
    public class GetPetByIdExample
    {
        public void main(){
            
            // Configure OAuth2 access token for authorization: petstore_auth
            Configuration.Default.AccessToken = 'YOUR_ACCESS_TOKEN';
            // Configure API key authorization: api_key
            Configuration.Default.ApiKey.Add('api_key', 'YOUR_API_KEY');
            // Uncomment below to setup prefix (e.g. BEARER) for API key, if needed
            // Configuration.Default.ApiKeyPrefix.Add('api_key', 'BEARER');

            var apiInstance = new PetApi();
            var petId = 789;  // long? | ID of pet that needs to be fetched

            try {
                Pet result = apiInstance.GetPetById(petId);
                Debug.WriteLine(result);
            } catch (Exception e) {
                Debug.Print("Exception when calling PetApi.GetPetById: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **long?**| ID of pet that needs to be fetched | 

### Return type

[**Pet**](Pet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth), [api_key](../README.md#api_key)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **GetPetByIdInObject**
> InlineResponse200 GetPetByIdInObject(petId)

Fake endpoint to test inline arbitrary object return by 'Find pet by ID'

Returns a pet when ID < 10.  ID > 10 or nonintegers will simulate API error conditions

### Example 
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Module;

namespace Example
{
    public class GetPetByIdInObjectExample
    {
        public void main(){
            
            // Configure OAuth2 access token for authorization: petstore_auth
            Configuration.Default.AccessToken = 'YOUR_ACCESS_TOKEN';
            // Configure API key authorization: api_key
            Configuration.Default.ApiKey.Add('api_key', 'YOUR_API_KEY');
            // Uncomment below to setup prefix (e.g. BEARER) for API key, if needed
            // Configuration.Default.ApiKeyPrefix.Add('api_key', 'BEARER');

            var apiInstance = new PetApi();
            var petId = 789;  // long? | ID of pet that needs to be fetched

            try {
                InlineResponse200 result = apiInstance.GetPetByIdInObject(petId);
                Debug.WriteLine(result);
            } catch (Exception e) {
                Debug.Print("Exception when calling PetApi.GetPetByIdInObject: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **long?**| ID of pet that needs to be fetched | 

### Return type

[**InlineResponse200**](InlineResponse200.md)

### Authorization

[petstore_auth](../README.md#petstore_auth), [api_key](../README.md#api_key)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **PetPetIdtestingByteArraytrueGet**
> byte[] PetPetIdtestingByteArraytrueGet(petId)

Fake endpoint to test byte array return by 'Find pet by ID'

Returns a pet when ID < 10.  ID > 10 or nonintegers will simulate API error conditions

### Example 
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Module;

namespace Example
{
    public class PetPetIdtestingByteArraytrueGetExample
    {
        public void main(){
            
            // Configure OAuth2 access token for authorization: petstore_auth
            Configuration.Default.AccessToken = 'YOUR_ACCESS_TOKEN';
            // Configure API key authorization: api_key
            Configuration.Default.ApiKey.Add('api_key', 'YOUR_API_KEY');
            // Uncomment below to setup prefix (e.g. BEARER) for API key, if needed
            // Configuration.Default.ApiKeyPrefix.Add('api_key', 'BEARER');

            var apiInstance = new PetApi();
            var petId = 789;  // long? | ID of pet that needs to be fetched

            try {
                byte[] result = apiInstance.PetPetIdtestingByteArraytrueGet(petId);
                Debug.WriteLine(result);
            } catch (Exception e) {
                Debug.Print("Exception when calling PetApi.PetPetIdtestingByteArraytrueGet: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **long?**| ID of pet that needs to be fetched | 

### Return type

**byte[]**

### Authorization

[petstore_auth](../README.md#petstore_auth), [api_key](../README.md#api_key)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **UpdatePet**
> UpdatePet(body)

Update an existing pet



### Example 
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Module;

namespace Example
{
    public class UpdatePetExample
    {
        public void main(){
            
            // Configure OAuth2 access token for authorization: petstore_auth
            Configuration.Default.AccessToken = 'YOUR_ACCESS_TOKEN';

            var apiInstance = new PetApi();
            var body = new Pet(); // Pet | Pet object that needs to be added to the store

            try {
                apiInstance.UpdatePet(body);
            } catch (Exception e) {
                Debug.Print("Exception when calling PetApi.UpdatePet: " + e.Message );
            }
        }
    }
}
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

# **UpdatePetWithForm**
> UpdatePetWithForm(petId, name, status)

Updates a pet in the store with form data



### Example 
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Module;

namespace Example
{
    public class UpdatePetWithFormExample
    {
        public void main(){
            
            // Configure OAuth2 access token for authorization: petstore_auth
            Configuration.Default.AccessToken = 'YOUR_ACCESS_TOKEN';

            var apiInstance = new PetApi();
            var petId = petId_example;  // string | ID of pet that needs to be updated
            var name = name_example;  // string | Updated name of the pet
            var status = status_example;  // string | Updated status of the pet

            try {
                apiInstance.UpdatePetWithForm(petId, name, status);
            } catch (Exception e) {
                Debug.Print("Exception when calling PetApi.UpdatePetWithForm: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **string**| ID of pet that needs to be updated | 
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

# **UploadFile**
> UploadFile(petId, additionalMetadata, file)

uploads an image



### Example 
```csharp
using System;
using System.Diagnostics;
using IO.Swagger.Api;
using IO.Swagger.Client;
using IO.Swagger.Module;

namespace Example
{
    public class UploadFileExample
    {
        public void main(){
            
            // Configure OAuth2 access token for authorization: petstore_auth
            Configuration.Default.AccessToken = 'YOUR_ACCESS_TOKEN';

            var apiInstance = new PetApi();
            var petId = 789;  // long? | ID of pet to update
            var additionalMetadata = additionalMetadata_example;  // string | Additional data to pass to server
            var file = new Stream(); // Stream | file to upload

            try {
                apiInstance.UploadFile(petId, additionalMetadata, file);
            } catch (Exception e) {
                Debug.Print("Exception when calling PetApi.UploadFile: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **long?**| ID of pet to update | 
 **additionalMetadata** | **string**| Additional data to pass to server | [optional] 
 **file** | **Stream**| file to upload | [optional] 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP reuqest headers

 - **Content-Type**: multipart/form-data
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

