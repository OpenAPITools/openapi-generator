# OpenAPI\Client\PetApi

All URIs are relative to *http://petstore.swagger.io:80/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**addPet**](PetApi.md#addPet) | **POST** /pet | Add a new pet to the store
[**deletePet**](PetApi.md#deletePet) | **DELETE** /pet/{petId} | Deletes a pet
[**findPetsByStatus**](PetApi.md#findPetsByStatus) | **GET** /pet/findByStatus | Finds Pets by status
[**findPetsByTags**](PetApi.md#findPetsByTags) | **GET** /pet/findByTags | Finds Pets by tags
[**getPetById**](PetApi.md#getPetById) | **GET** /pet/{petId} | Find pet by ID
[**updatePet**](PetApi.md#updatePet) | **PUT** /pet | Update an existing pet
[**updatePetWithForm**](PetApi.md#updatePetWithForm) | **POST** /pet/{petId} | Updates a pet in the store with form data
[**uploadFile**](PetApi.md#uploadFile) | **POST** /pet/{petId}/uploadImage | uploads an image
[**uploadFileWithRequiredFile**](PetApi.md#uploadFileWithRequiredFile) | **POST** /fake/{petId}/uploadImageWithRequiredFile | uploads an image (required)



## addPet

> addPet($pet)

Add a new pet to the store

### Example

```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');


// Configure OAuth2 access token for authorization: petstore_auth
$config = OpenAPI\Client\Configuration::getDefaultConfiguration()->setAccessToken('YOUR_ACCESS_TOKEN');


$apiInstance = new OpenAPI\Client\Api\PetApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client(),
    $config
);
$pet = new \OpenAPI\Client\Model\Pet(); // \OpenAPI\Client\Model\Pet | Pet object that needs to be added to the store

try {
    $apiInstance->addPet($pet);
} catch (Exception $e) {
    echo 'Exception when calling PetApi->addPet: ', $e->getMessage(), PHP_EOL;
}
?>
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet** | [**\OpenAPI\Client\Model\Pet**](../Model/Pet.md)| Pet object that needs to be added to the store |

### Return type

void (empty response body)

### Authorization

[petstore_auth](../../README.md#petstore_auth)

### HTTP request headers

- **Content-Type**: application/json, application/xml
- **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../../README.md#documentation-for-models)
[[Back to README]](../../README.md)


## deletePet

> deletePet($pet_id, $api_key)

Deletes a pet

### Example

```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');


// Configure OAuth2 access token for authorization: petstore_auth
$config = OpenAPI\Client\Configuration::getDefaultConfiguration()->setAccessToken('YOUR_ACCESS_TOKEN');


$apiInstance = new OpenAPI\Client\Api\PetApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client(),
    $config
);
$pet_id = 56; // int | Pet id to delete
$api_key = 'api_key_example'; // string | 

try {
    $apiInstance->deletePet($pet_id, $api_key);
} catch (Exception $e) {
    echo 'Exception when calling PetApi->deletePet: ', $e->getMessage(), PHP_EOL;
}
?>
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet_id** | **int**| Pet id to delete |
 **api_key** | **string**|  | [optional]

### Return type

void (empty response body)

### Authorization

[petstore_auth](../../README.md#petstore_auth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../../README.md#documentation-for-models)
[[Back to README]](../../README.md)


## findPetsByStatus

> \OpenAPI\Client\Model\Pet[] findPetsByStatus($status)

Finds Pets by status

Multiple status values can be provided with comma separated strings

### Example

```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');


// Configure OAuth2 access token for authorization: petstore_auth
$config = OpenAPI\Client\Configuration::getDefaultConfiguration()->setAccessToken('YOUR_ACCESS_TOKEN');


$apiInstance = new OpenAPI\Client\Api\PetApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client(),
    $config
);
$status = array('status_example'); // string[] | Status values that need to be considered for filter

try {
    $result = $apiInstance->findPetsByStatus($status);
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling PetApi->findPetsByStatus: ', $e->getMessage(), PHP_EOL;
}
?>
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **status** | [**string[]**](../Model/string.md)| Status values that need to be considered for filter |

### Return type

[**\OpenAPI\Client\Model\Pet[]**](../Model/Pet.md)

### Authorization

[petstore_auth](../../README.md#petstore_auth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../../README.md#documentation-for-models)
[[Back to README]](../../README.md)


## findPetsByTags

> \OpenAPI\Client\Model\Pet[] findPetsByTags($tags)

Finds Pets by tags

Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.

### Example

```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');


// Configure OAuth2 access token for authorization: petstore_auth
$config = OpenAPI\Client\Configuration::getDefaultConfiguration()->setAccessToken('YOUR_ACCESS_TOKEN');


$apiInstance = new OpenAPI\Client\Api\PetApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client(),
    $config
);
$tags = array('tags_example'); // string[] | Tags to filter by

try {
    $result = $apiInstance->findPetsByTags($tags);
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling PetApi->findPetsByTags: ', $e->getMessage(), PHP_EOL;
}
?>
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **tags** | [**string[]**](../Model/string.md)| Tags to filter by |

### Return type

[**\OpenAPI\Client\Model\Pet[]**](../Model/Pet.md)

### Authorization

[petstore_auth](../../README.md#petstore_auth)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../../README.md#documentation-for-models)
[[Back to README]](../../README.md)


## getPetById

> \OpenAPI\Client\Model\Pet getPetById($pet_id)

Find pet by ID

Returns a single pet

### Example

```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');


// Configure API key authorization: api_key
$config = OpenAPI\Client\Configuration::getDefaultConfiguration()->setApiKey('api_key', 'YOUR_API_KEY');
// Uncomment below to setup prefix (e.g. Bearer) for API key, if needed
// $config = OpenAPI\Client\Configuration::getDefaultConfiguration()->setApiKeyPrefix('api_key', 'Bearer');


$apiInstance = new OpenAPI\Client\Api\PetApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client(),
    $config
);
$pet_id = 56; // int | ID of pet to return

try {
    $result = $apiInstance->getPetById($pet_id);
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling PetApi->getPetById: ', $e->getMessage(), PHP_EOL;
}
?>
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet_id** | **int**| ID of pet to return |

### Return type

[**\OpenAPI\Client\Model\Pet**](../Model/Pet.md)

### Authorization

[api_key](../../README.md#api_key)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../../README.md#documentation-for-models)
[[Back to README]](../../README.md)


## updatePet

> updatePet($pet)

Update an existing pet

### Example

```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');


// Configure OAuth2 access token for authorization: petstore_auth
$config = OpenAPI\Client\Configuration::getDefaultConfiguration()->setAccessToken('YOUR_ACCESS_TOKEN');


$apiInstance = new OpenAPI\Client\Api\PetApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client(),
    $config
);
$pet = new \OpenAPI\Client\Model\Pet(); // \OpenAPI\Client\Model\Pet | Pet object that needs to be added to the store

try {
    $apiInstance->updatePet($pet);
} catch (Exception $e) {
    echo 'Exception when calling PetApi->updatePet: ', $e->getMessage(), PHP_EOL;
}
?>
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet** | [**\OpenAPI\Client\Model\Pet**](../Model/Pet.md)| Pet object that needs to be added to the store |

### Return type

void (empty response body)

### Authorization

[petstore_auth](../../README.md#petstore_auth)

### HTTP request headers

- **Content-Type**: application/json, application/xml
- **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../../README.md#documentation-for-models)
[[Back to README]](../../README.md)


## updatePetWithForm

> updatePetWithForm($pet_id, $name, $status)

Updates a pet in the store with form data

### Example

```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');


// Configure OAuth2 access token for authorization: petstore_auth
$config = OpenAPI\Client\Configuration::getDefaultConfiguration()->setAccessToken('YOUR_ACCESS_TOKEN');


$apiInstance = new OpenAPI\Client\Api\PetApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client(),
    $config
);
$pet_id = 56; // int | ID of pet that needs to be updated
$name = 'name_example'; // string | Updated name of the pet
$status = 'status_example'; // string | Updated status of the pet

try {
    $apiInstance->updatePetWithForm($pet_id, $name, $status);
} catch (Exception $e) {
    echo 'Exception when calling PetApi->updatePetWithForm: ', $e->getMessage(), PHP_EOL;
}
?>
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

[petstore_auth](../../README.md#petstore_auth)

### HTTP request headers

- **Content-Type**: application/x-www-form-urlencoded
- **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../../README.md#documentation-for-models)
[[Back to README]](../../README.md)


## uploadFile

> \OpenAPI\Client\Model\ApiResponse uploadFile($pet_id, $additional_metadata, $file)

uploads an image

### Example

```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');


// Configure OAuth2 access token for authorization: petstore_auth
$config = OpenAPI\Client\Configuration::getDefaultConfiguration()->setAccessToken('YOUR_ACCESS_TOKEN');


$apiInstance = new OpenAPI\Client\Api\PetApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client(),
    $config
);
$pet_id = 56; // int | ID of pet to update
$additional_metadata = 'additional_metadata_example'; // string | Additional data to pass to server
$file = "/path/to/file.txt"; // \SplFileObject | file to upload

try {
    $result = $apiInstance->uploadFile($pet_id, $additional_metadata, $file);
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling PetApi->uploadFile: ', $e->getMessage(), PHP_EOL;
}
?>
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet_id** | **int**| ID of pet to update |
 **additional_metadata** | **string**| Additional data to pass to server | [optional]
 **file** | **\SplFileObject****\SplFileObject**| file to upload | [optional]

### Return type

[**\OpenAPI\Client\Model\ApiResponse**](../Model/ApiResponse.md)

### Authorization

[petstore_auth](../../README.md#petstore_auth)

### HTTP request headers

- **Content-Type**: multipart/form-data
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../../README.md#documentation-for-models)
[[Back to README]](../../README.md)


## uploadFileWithRequiredFile

> \OpenAPI\Client\Model\ApiResponse uploadFileWithRequiredFile($pet_id, $required_file, $additional_metadata)

uploads an image (required)

### Example

```php
<?php
require_once(__DIR__ . '/vendor/autoload.php');


// Configure OAuth2 access token for authorization: petstore_auth
$config = OpenAPI\Client\Configuration::getDefaultConfiguration()->setAccessToken('YOUR_ACCESS_TOKEN');


$apiInstance = new OpenAPI\Client\Api\PetApi(
    // If you want use custom http client, pass your client which implements `GuzzleHttp\ClientInterface`.
    // This is optional, `GuzzleHttp\Client` will be used as default.
    new GuzzleHttp\Client(),
    $config
);
$pet_id = 56; // int | ID of pet to update
$required_file = "/path/to/file.txt"; // \SplFileObject | file to upload
$additional_metadata = 'additional_metadata_example'; // string | Additional data to pass to server

try {
    $result = $apiInstance->uploadFileWithRequiredFile($pet_id, $required_file, $additional_metadata);
    print_r($result);
} catch (Exception $e) {
    echo 'Exception when calling PetApi->uploadFileWithRequiredFile: ', $e->getMessage(), PHP_EOL;
}
?>
```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet_id** | **int**| ID of pet to update |
 **required_file** | **\SplFileObject****\SplFileObject**| file to upload |
 **additional_metadata** | **string**| Additional data to pass to server | [optional]

### Return type

[**\OpenAPI\Client\Model\ApiResponse**](../Model/ApiResponse.md)

### Authorization

[petstore_auth](../../README.md#petstore_auth)

### HTTP request headers

- **Content-Type**: multipart/form-data
- **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints)
[[Back to Model list]](../../README.md#documentation-for-models)
[[Back to README]](../../README.md)

