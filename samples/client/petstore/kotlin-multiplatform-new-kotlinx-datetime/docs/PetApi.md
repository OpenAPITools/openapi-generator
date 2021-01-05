# PetApi

All URIs are by default relative to `http://petstore.swagger.io/v2`

Method | HTTP request | Description
------------- | ------------- | -------------
[`addPet`](#addPet) | `POST` /pet | Add a new pet to the store
[`deletePet`](#deletePet) | `DELETE` /pet/{petId} | Deletes a pet
[`findPetsByStatus`](#findPetsByStatus) | `GET` /pet/findByStatus | Finds Pets by status
[`findPetsByTags`](#findPetsByTags) | `GET` /pet/findByTags | Finds Pets by tags
[`getPetById`](#getPetById) | `GET` /pet/{petId} | Find pet by ID
[`updatePet`](#updatePet) | `PUT` /pet | Update an existing pet
[`updatePetWithForm`](#updatePetWithForm) | `POST` /pet/{petId} | Updates a pet in the store with form data
[`uploadFile`](#uploadFile) | `POST` /pet/{petId}/uploadImage | uploads an image


# **addPet**
> addPet(body)

Add a new pet to the store

### Example
```kotlin
// Import classes:
import org.openapitools.client.apis.PetApi
import org.openapitools.client.infrastructure.*
import org.openapitools.client.models.*

val petApi = PetApi()
val body: Pet =  
try {
    petApi.addPet(
        body,
    )
} catch (e: ClientException) {
    println("4xx response calling PetApi#addPet")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling PetApi#addPet")
    e.printStackTrace()
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Pet**](Pet.md)| Pet object that needs to be added to the store |

### Return type

`Unit`

### Authorization


Configure petstore_auth (OAuth):
```kotlin
apiClient.oAuth["petstore_auth"].accessToken = ""
```

### HTTP request headers

 - **Content-Type**: application/jsonapplication/xml
 - **Accept**: Not defined

# **deletePet**
> deletePet(petId, apiKey)

Deletes a pet

### Example
```kotlin
// Import classes:
import org.openapitools.client.apis.PetApi
import org.openapitools.client.infrastructure.*
import org.openapitools.client.models.*

val petApi = PetApi()
val petId: kotlin.Long = 789 
val apiKey: kotlin.String = apiKey_example // Optional
try {
    petApi.deletePet(
        petId,
        apiKey = apiKey,
    )
} catch (e: ClientException) {
    println("4xx response calling PetApi#deletePet")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling PetApi#deletePet")
    e.printStackTrace()
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **kotlin.Long**| Pet id to delete |
 **apiKey** | **kotlin.String**|  | [optional]

### Return type

`Unit`

### Authorization


Configure petstore_auth (OAuth):
```kotlin
apiClient.oAuth["petstore_auth"].accessToken = ""
```

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

# **findPetsByStatus**
> kotlin.collections.List&lt;Pet&gt; findPetsByStatus(status)

Finds Pets by status

Multiple status values can be provided with comma separated strings

### Example
```kotlin
// Import classes:
import org.openapitools.client.apis.PetApi
import org.openapitools.client.infrastructure.*
import org.openapitools.client.models.*

val petApi = PetApi()
val status: kotlin.collections.List<kotlin.String> =  
try {
    val result: kotlin.collections.List<Pet> = petApi.findPetsByStatus(
        status,
    )
    println(result)
} catch (e: ClientException) {
    println("4xx response calling PetApi#findPetsByStatus")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling PetApi#findPetsByStatus")
    e.printStackTrace()
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **status** | [**kotlin.collections.List&lt;kotlin.String&gt;**](kotlin.String.md)| Status values that need to be considered for filter | [enum: available, pending, sold]

### Return type

[`kotlin.collections.List&lt;Pet&gt;`](Pet.md)

### Authorization


Configure petstore_auth (OAuth):
```kotlin
apiClient.oAuth["petstore_auth"].accessToken = ""
```

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xmlapplication/json

# **findPetsByTags**
> kotlin.collections.List&lt;Pet&gt; findPetsByTags(tags)

Finds Pets by tags

Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.

### Example
```kotlin
// Import classes:
import org.openapitools.client.apis.PetApi
import org.openapitools.client.infrastructure.*
import org.openapitools.client.models.*

val petApi = PetApi()
val tags: kotlin.collections.List<kotlin.String> =  
try {
    val result: kotlin.collections.List<Pet> = petApi.findPetsByTags(
        tags,
    )
    println(result)
} catch (e: ClientException) {
    println("4xx response calling PetApi#findPetsByTags")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling PetApi#findPetsByTags")
    e.printStackTrace()
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **tags** | [**kotlin.collections.List&lt;kotlin.String&gt;**](kotlin.String.md)| Tags to filter by |

### Return type

[`kotlin.collections.List&lt;Pet&gt;`](Pet.md)

### Authorization


Configure petstore_auth (OAuth):
```kotlin
apiClient.oAuth["petstore_auth"].accessToken = ""
```

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xmlapplication/json

# **getPetById**
> Pet getPetById(petId)

Find pet by ID

Returns a single pet

### Example
```kotlin
// Import classes:
import org.openapitools.client.apis.PetApi
import org.openapitools.client.infrastructure.*
import org.openapitools.client.models.*

val petApi = PetApi()
val petId: kotlin.Long = 789 
try {
    val result: Pet = petApi.getPetById(
        petId,
    )
    println(result)
} catch (e: ClientException) {
    println("4xx response calling PetApi#getPetById")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling PetApi#getPetById")
    e.printStackTrace()
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **kotlin.Long**| ID of pet to return |

### Return type

[`Pet`](Pet.md)

### Authorization


CodegenSecurity{name&#x3D;&#39;api_key&#39;, type&#x3D;&#39;apiKey&#39;, scheme&#x3D;&#39;null&#39;, isBasic&#x3D;false, isOAuth&#x3D;false, isApiKey&#x3D;true, isBasicBasic&#x3D;false, isHttpSignature&#x3D;false, isBasicBearer&#x3D;false, bearerFormat&#x3D;&#39;null&#39;, vendorExtensions&#x3D;{}, keyParamName&#x3D;&#39;api_key&#39;, isKeyInQuery&#x3D;false, isKeyInHeader&#x3D;true, isKeyInCookie&#x3D;false, flow&#x3D;&#39;null&#39;, authorizationUrl&#x3D;&#39;null&#39;, tokenUrl&#x3D;&#39;null&#39;, scopes&#x3D;null, isCode&#x3D;false, isPassword&#x3D;false, isApplication&#x3D;false, isImplicit&#x3D;false}
Configure api_key (API key):
```kotlin
apiClient.apiKeyAuth["api_key"].apply {
    key = ""
    keyPrefix = ""
}
```

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xmlapplication/json

# **updatePet**
> updatePet(body)

Update an existing pet

### Example
```kotlin
// Import classes:
import org.openapitools.client.apis.PetApi
import org.openapitools.client.infrastructure.*
import org.openapitools.client.models.*

val petApi = PetApi()
val body: Pet =  
try {
    petApi.updatePet(
        body,
    )
} catch (e: ClientException) {
    println("4xx response calling PetApi#updatePet")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling PetApi#updatePet")
    e.printStackTrace()
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Pet**](Pet.md)| Pet object that needs to be added to the store |

### Return type

`Unit`

### Authorization


Configure petstore_auth (OAuth):
```kotlin
apiClient.oAuth["petstore_auth"].accessToken = ""
```

### HTTP request headers

 - **Content-Type**: application/jsonapplication/xml
 - **Accept**: Not defined

# **updatePetWithForm**
> updatePetWithForm(petId, name, status)

Updates a pet in the store with form data

### Example
```kotlin
// Import classes:
import org.openapitools.client.apis.PetApi
import org.openapitools.client.infrastructure.*
import org.openapitools.client.models.*

val petApi = PetApi()
val petId: kotlin.Long = 789 
val name: kotlin.String = name_example // Optional
val status: kotlin.String = status_example // Optional
try {
    petApi.updatePetWithForm(
        petId,
        name = name,
        status = status,
    )
} catch (e: ClientException) {
    println("4xx response calling PetApi#updatePetWithForm")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling PetApi#updatePetWithForm")
    e.printStackTrace()
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **kotlin.Long**| ID of pet that needs to be updated |
 **name** | **kotlin.String**| Updated name of the pet | [optional]
 **status** | **kotlin.String**| Updated status of the pet | [optional]

### Return type

`Unit`

### Authorization


Configure petstore_auth (OAuth):
```kotlin
apiClient.oAuth["petstore_auth"].accessToken = ""
```

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

# **uploadFile**
> ApiResponse uploadFile(petId, additionalMetadata, file)

uploads an image

### Example
```kotlin
// Import classes:
import org.openapitools.client.apis.PetApi
import org.openapitools.client.infrastructure.*
import org.openapitools.client.models.*

val petApi = PetApi()
val petId: kotlin.Long = 789 
val additionalMetadata: kotlin.String = additionalMetadata_example // Optional
val file: io.ktor.client.request.forms.InputProvider = BINARY_DATA_HERE // Optional
try {
    val result: ApiResponse = petApi.uploadFile(
        petId,
        additionalMetadata = additionalMetadata,
        file = file,
    )
    println(result)
} catch (e: ClientException) {
    println("4xx response calling PetApi#uploadFile")
    e.printStackTrace()
} catch (e: ServerException) {
    println("5xx response calling PetApi#uploadFile")
    e.printStackTrace()
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **kotlin.Long**| ID of pet to update |
 **additionalMetadata** | **kotlin.String**| Additional data to pass to server | [optional]
 **file** | **io.ktor.client.request.forms.InputProvider**| file to upload | [optional]

### Return type

[`ApiResponse`](ApiResponse.md)

### Authorization


Configure petstore_auth (OAuth):
```kotlin
apiClient.oAuth["petstore_auth"].accessToken = ""
```

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: application/json

