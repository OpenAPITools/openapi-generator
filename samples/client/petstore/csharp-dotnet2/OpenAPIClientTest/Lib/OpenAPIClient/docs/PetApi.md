# Org.OpenAPITools.Api.PetApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**AddPet**](PetApi.md#addpet) | **POST** /pet | Add a new pet to the store
[**DeletePet**](PetApi.md#deletepet) | **DELETE** /pet/{petId} | Deletes a pet
[**FindPetsByStatus**](PetApi.md#findpetsbystatus) | **GET** /pet/findByStatus | Finds Pets by status
[**FindPetsByTags**](PetApi.md#findpetsbytags) | **GET** /pet/findByTags | Finds Pets by tags
[**GetPetById**](PetApi.md#getpetbyid) | **GET** /pet/{petId} | Find pet by ID
[**UpdatePet**](PetApi.md#updatepet) | **PUT** /pet | Update an existing pet
[**UpdatePetWithForm**](PetApi.md#updatepetwithform) | **POST** /pet/{petId} | Updates a pet in the store with form data
[**UploadFile**](PetApi.md#uploadfile) | **POST** /pet/{petId}/uploadImage | uploads an image


<a name="addpet"></a>
# **AddPet**
> void AddPet (Pet body)

Add a new pet to the store

### Example
```csharp
using System;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class AddPetExample
    {
        public void main()
        {
            // Configure OAuth2 access token for authorization: petstore_auth
            Configuration.Default.AccessToken = "YOUR_ACCESS_TOKEN";

            var apiInstance = new PetApi();
            var body = new Pet(); // Pet | Pet object that needs to be added to the store

            try
            {
                // Add a new pet to the store
                apiInstance.AddPet(body);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling PetApi.AddPet: " + e.Message );
            }
        }
    }
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
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="deletepet"></a>
# **DeletePet**
> void DeletePet (long? petId, string apiKey)

Deletes a pet

### Example
```csharp
using System;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class DeletePetExample
    {
        public void main()
        {
            // Configure OAuth2 access token for authorization: petstore_auth
            Configuration.Default.AccessToken = "YOUR_ACCESS_TOKEN";

            var apiInstance = new PetApi();
            var petId = 789;  // long? | Pet id to delete
            var apiKey = apiKey_example;  // string |  (optional) 

            try
            {
                // Deletes a pet
                apiInstance.DeletePet(petId, apiKey);
            }
            catch (Exception e)
            {
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

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="findpetsbystatus"></a>
# **FindPetsByStatus**
> List<Pet> FindPetsByStatus (List<string> status)

Finds Pets by status

Multiple status values can be provided with comma separated strings

### Example
```csharp
using System;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class FindPetsByStatusExample
    {
        public void main()
        {
            // Configure OAuth2 access token for authorization: petstore_auth
            Configuration.Default.AccessToken = "YOUR_ACCESS_TOKEN";

            var apiInstance = new PetApi();
            var status = status_example;  // List<string> | Status values that need to be considered for filter

            try
            {
                // Finds Pets by status
                List&lt;Pet&gt; result = apiInstance.FindPetsByStatus(status);
                Debug.WriteLine(result);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling PetApi.FindPetsByStatus: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **status** | **List<string>**| Status values that need to be considered for filter | 

### Return type

[**List<Pet>**](Pet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="findpetsbytags"></a>
# **FindPetsByTags**
> List<Pet> FindPetsByTags (List<string> tags)

Finds Pets by tags

Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.

### Example
```csharp
using System;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class FindPetsByTagsExample
    {
        public void main()
        {
            // Configure OAuth2 access token for authorization: petstore_auth
            Configuration.Default.AccessToken = "YOUR_ACCESS_TOKEN";

            var apiInstance = new PetApi();
            var tags = new List<string>(); // List<string> | Tags to filter by

            try
            {
                // Finds Pets by tags
                List&lt;Pet&gt; result = apiInstance.FindPetsByTags(tags);
                Debug.WriteLine(result);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling PetApi.FindPetsByTags: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **tags** | [**List<string>**](string.md)| Tags to filter by | 

### Return type

[**List<Pet>**](Pet.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="getpetbyid"></a>
# **GetPetById**
> Pet GetPetById (long? petId)

Find pet by ID

Returns a single pet

### Example
```csharp
using System;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class GetPetByIdExample
    {
        public void main()
        {
            // Configure API key authorization: api_key
            Configuration.Default.ApiKey.Add("api_key", "YOUR_API_KEY");
            // Uncomment below to setup prefix (e.g. Bearer) for API key, if needed
            // Configuration.Default.ApiKeyPrefix.Add("api_key", "Bearer");

            var apiInstance = new PetApi();
            var petId = 789;  // long? | ID of pet to return

            try
            {
                // Find pet by ID
                Pet result = apiInstance.GetPetById(petId);
                Debug.WriteLine(result);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling PetApi.GetPetById: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **long?**| ID of pet to return | 

### Return type

[**Pet**](Pet.md)

### Authorization

[api_key](../README.md#api_key)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="updatepet"></a>
# **UpdatePet**
> void UpdatePet (Pet body)

Update an existing pet

### Example
```csharp
using System;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class UpdatePetExample
    {
        public void main()
        {
            // Configure OAuth2 access token for authorization: petstore_auth
            Configuration.Default.AccessToken = "YOUR_ACCESS_TOKEN";

            var apiInstance = new PetApi();
            var body = new Pet(); // Pet | Pet object that needs to be added to the store

            try
            {
                // Update an existing pet
                apiInstance.UpdatePet(body);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling PetApi.UpdatePet: " + e.Message );
            }
        }
    }
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
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="updatepetwithform"></a>
# **UpdatePetWithForm**
> void UpdatePetWithForm (long? petId, string name, string status)

Updates a pet in the store with form data

### Example
```csharp
using System;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class UpdatePetWithFormExample
    {
        public void main()
        {
            // Configure OAuth2 access token for authorization: petstore_auth
            Configuration.Default.AccessToken = "YOUR_ACCESS_TOKEN";

            var apiInstance = new PetApi();
            var petId = 789;  // long? | ID of pet that needs to be updated
            var name = name_example;  // string | Updated name of the pet (optional) 
            var status = status_example;  // string | Updated status of the pet (optional) 

            try
            {
                // Updates a pet in the store with form data
                apiInstance.UpdatePetWithForm(petId, name, status);
            }
            catch (Exception e)
            {
                Debug.Print("Exception when calling PetApi.UpdatePetWithForm: " + e.Message );
            }
        }
    }
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **long?**| ID of pet that needs to be updated | 
 **name** | **string**| Updated name of the pet | [optional] 
 **status** | **string**| Updated status of the pet | [optional] 

### Return type

void (empty response body)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

<a name="uploadfile"></a>
# **UploadFile**
> ApiResponse UploadFile (long? petId, string additionalMetadata, System.IO.Stream file)

uploads an image

### Example
```csharp
using System;
using System.Diagnostics;
using Org.OpenAPITools.Api;
using Org.OpenAPITools.Client;
using Org.OpenAPITools.Model;

namespace Example
{
    public class UploadFileExample
    {
        public void main()
        {
            // Configure OAuth2 access token for authorization: petstore_auth
            Configuration.Default.AccessToken = "YOUR_ACCESS_TOKEN";

            var apiInstance = new PetApi();
            var petId = 789;  // long? | ID of pet to update
            var additionalMetadata = additionalMetadata_example;  // string | Additional data to pass to server (optional) 
            var file = new System.IO.Stream(); // System.IO.Stream | file to upload (optional) 

            try
            {
                // uploads an image
                ApiResponse result = apiInstance.UploadFile(petId, additionalMetadata, file);
                Debug.WriteLine(result);
            }
            catch (Exception e)
            {
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
 **file** | **System.IO.Stream**| file to upload | [optional] 

### Return type

[**ApiResponse**](ApiResponse.md)

### Authorization

[petstore_auth](../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

