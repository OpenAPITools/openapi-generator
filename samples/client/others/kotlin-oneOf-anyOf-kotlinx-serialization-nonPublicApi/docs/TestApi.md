# TestApi

All URIs are relative to *http://example.org*

| Method | HTTP request | Description |
| ------------- | ------------- | ------------- |
| [**getAnyOf**](TestApi.md#getAnyOf) | **GET** v1/test/anyOf |  |
| [**getAnyOfArray**](TestApi.md#getAnyOfArray) | **GET** v1/test/anyOfArray |  |
| [**getOneOf**](TestApi.md#getOneOf) | **GET** v1/test/oneOf |  |
| [**getOneOfArray**](TestApi.md#getOneOfArray) | **GET** v1/test/oneOfArray |  |
| [**getOneOfBooleanPrimitive**](TestApi.md#getOneOfBooleanPrimitive) | **GET** v1/test/oneOfBooleanPrimitive |  |
| [**getOneOfPrimitive**](TestApi.md#getOneOfPrimitive) | **GET** v1/test/oneOfPrimitive |  |





### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(TestApi::class.java)

val result : AnyOfUserOrPet = webService.getAnyOf()
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**AnyOfUserOrPet**](AnyOfUserOrPet.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json




### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(TestApi::class.java)

val result : AnyOfUserOrPetOrArrayString = webService.getAnyOfArray()
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**AnyOfUserOrPetOrArrayString**](AnyOfUserOrPetOrArrayString.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json




### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(TestApi::class.java)

val result : UserOrPet = webService.getOneOf()
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**UserOrPet**](UserOrPet.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json




### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(TestApi::class.java)

val result : UserOrPetOrArrayString = webService.getOneOfArray()
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**UserOrPetOrArrayString**](UserOrPetOrArrayString.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json




### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(TestApi::class.java)

val result : BooleanOrLong = webService.getOneOfBooleanPrimitive()
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**BooleanOrLong**](BooleanOrLong.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json




### Example
```kotlin
// Import classes:
//import org.openapitools.client.*
//import org.openapitools.client.infrastructure.*
//import org.openapitools.client.models.*

val apiClient = ApiClient()
val webService = apiClient.createWebservice(TestApi::class.java)

val result : StringOrLong = webService.getOneOfPrimitive()
```

### Parameters
This endpoint does not need any parameter.

### Return type

[**StringOrLong**](StringOrLong.md)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/json

