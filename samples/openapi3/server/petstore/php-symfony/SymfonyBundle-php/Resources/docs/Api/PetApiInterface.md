# OpenAPI\Server\Api\PetApiInterface

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**addPet**](PetApiInterface.md#addPet) | **POST** /pet | Add a new pet to the store
[**deletePet**](PetApiInterface.md#deletePet) | **DELETE** /pet/{petId} | Deletes a pet
[**findPetsByStatus**](PetApiInterface.md#findPetsByStatus) | **GET** /pet/findByStatus | Finds Pets by status
[**findPetsByTags**](PetApiInterface.md#findPetsByTags) | **GET** /pet/findByTags | Finds Pets by tags
[**getPetById**](PetApiInterface.md#getPetById) | **GET** /pet/{petId} | Find pet by ID
[**updatePet**](PetApiInterface.md#updatePet) | **PUT** /pet | Update an existing pet
[**updatePetWithForm**](PetApiInterface.md#updatePetWithForm) | **POST** /pet/{petId} | Updates a pet in the store with form data
[**uploadFile**](PetApiInterface.md#uploadFile) | **POST** /pet/{petId}/uploadImage | uploads an image


## Service Declaration
```yaml
# src/Acme/MyBundle/Resources/services.yml
services:
    # ...
    acme.my_bundle.api.pet:
        class: Acme\MyBundle\Api\PetApi
        tags:
            - { name: "open_api_server.api", api: "pet" }
    # ...
```

## **addPet**
> addPet($pet)

Add a new pet to the store

### Example Implementation
```php
<?php
// src/Acme/MyBundle/Api/PetApiInterface.php

namespace Acme\MyBundle\Api;

use OpenAPI\Server\Api\PetApiInterface;

class PetApi implements PetApiInterface
{

    /**
     * Configure OAuth2 access token for authorization: petstore_auth
     */
    public function setpetstore_auth($oauthToken)
    {
        // Retrieve logged in user from $oauthToken ...
    }

    // ...

    /**
     * Implementation of PetApiInterface#addPet
     */
    public function addPet(Pet $pet)
    {
        // Implement the operation ...
    }

    // ...
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet** | [**OpenAPI\Server\Model\Pet**](../Model/Pet.md)| Pet object that needs to be added to the store |

### Return type

void (empty response body)

### Authorization

[petstore_auth](../../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

## **deletePet**
> deletePet($petId, $apiKey)

Deletes a pet

### Example Implementation
```php
<?php
// src/Acme/MyBundle/Api/PetApiInterface.php

namespace Acme\MyBundle\Api;

use OpenAPI\Server\Api\PetApiInterface;

class PetApi implements PetApiInterface
{

    /**
     * Configure OAuth2 access token for authorization: petstore_auth
     */
    public function setpetstore_auth($oauthToken)
    {
        // Retrieve logged in user from $oauthToken ...
    }

    // ...

    /**
     * Implementation of PetApiInterface#deletePet
     */
    public function deletePet($petId, $apiKey = null)
    {
        // Implement the operation ...
    }

    // ...
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **int**| Pet id to delete |
 **apiKey** | **string**|  | [optional]

### Return type

void (empty response body)

### Authorization

[petstore_auth](../../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

## **findPetsByStatus**
> OpenAPI\Server\Model\Pet findPetsByStatus($status)

Finds Pets by status

Multiple status values can be provided with comma separated strings

### Example Implementation
```php
<?php
// src/Acme/MyBundle/Api/PetApiInterface.php

namespace Acme\MyBundle\Api;

use OpenAPI\Server\Api\PetApiInterface;

class PetApi implements PetApiInterface
{

    /**
     * Configure OAuth2 access token for authorization: petstore_auth
     */
    public function setpetstore_auth($oauthToken)
    {
        // Retrieve logged in user from $oauthToken ...
    }

    // ...

    /**
     * Implementation of PetApiInterface#findPetsByStatus
     */
    public function findPetsByStatus(array $status)
    {
        // Implement the operation ...
    }

    // ...
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **status** | [**string**](../Model/string.md)| Status values that need to be considered for filter |

### Return type

[**OpenAPI\Server\Model\Pet**](../Model/Pet.md)

### Authorization

[petstore_auth](../../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

## **findPetsByTags**
> OpenAPI\Server\Model\Pet findPetsByTags($tags, $maxCount)

Finds Pets by tags

Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.

### Example Implementation
```php
<?php
// src/Acme/MyBundle/Api/PetApiInterface.php

namespace Acme\MyBundle\Api;

use OpenAPI\Server\Api\PetApiInterface;

class PetApi implements PetApiInterface
{

    /**
     * Configure OAuth2 access token for authorization: petstore_auth
     */
    public function setpetstore_auth($oauthToken)
    {
        // Retrieve logged in user from $oauthToken ...
    }

    // ...

    /**
     * Implementation of PetApiInterface#findPetsByTags
     */
    public function findPetsByTags(array $tags, $maxCount = null)
    {
        // Implement the operation ...
    }

    // ...
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **tags** | [**string**](../Model/string.md)| Tags to filter by |
 **maxCount** | **int**| Maximum number of items to return | [optional]

### Return type

[**OpenAPI\Server\Model\Pet**](../Model/Pet.md)

### Authorization

[petstore_auth](../../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

## **getPetById**
> OpenAPI\Server\Model\Pet getPetById($petId)

Find pet by ID

Returns a single pet

### Example Implementation
```php
<?php
// src/Acme/MyBundle/Api/PetApiInterface.php

namespace Acme\MyBundle\Api;

use OpenAPI\Server\Api\PetApiInterface;

class PetApi implements PetApiInterface
{

    /**
     * Configure API key authorization: api_key
     */
    public function setapi_key($apiKey)
    {
        // Retrieve logged in user from $apiKey ...
    }

    // ...

    /**
     * Implementation of PetApiInterface#getPetById
     */
    public function getPetById($petId)
    {
        // Implement the operation ...
    }

    // ...
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **int**| ID of pet to return |

### Return type

[**OpenAPI\Server\Model\Pet**](../Model/Pet.md)

### Authorization

[api_key](../../README.md#api_key)

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

## **updatePet**
> updatePet($pet)

Update an existing pet

### Example Implementation
```php
<?php
// src/Acme/MyBundle/Api/PetApiInterface.php

namespace Acme\MyBundle\Api;

use OpenAPI\Server\Api\PetApiInterface;

class PetApi implements PetApiInterface
{

    /**
     * Configure OAuth2 access token for authorization: petstore_auth
     */
    public function setpetstore_auth($oauthToken)
    {
        // Retrieve logged in user from $oauthToken ...
    }

    // ...

    /**
     * Implementation of PetApiInterface#updatePet
     */
    public function updatePet(Pet $pet)
    {
        // Implement the operation ...
    }

    // ...
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **pet** | [**OpenAPI\Server\Model\Pet**](../Model/Pet.md)| Pet object that needs to be added to the store |

### Return type

void (empty response body)

### Authorization

[petstore_auth](../../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/json, application/xml
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

## **updatePetWithForm**
> updatePetWithForm($petId, $name, $status)

Updates a pet in the store with form data

### Example Implementation
```php
<?php
// src/Acme/MyBundle/Api/PetApiInterface.php

namespace Acme\MyBundle\Api;

use OpenAPI\Server\Api\PetApiInterface;

class PetApi implements PetApiInterface
{

    /**
     * Configure OAuth2 access token for authorization: petstore_auth
     */
    public function setpetstore_auth($oauthToken)
    {
        // Retrieve logged in user from $oauthToken ...
    }

    // ...

    /**
     * Implementation of PetApiInterface#updatePetWithForm
     */
    public function updatePetWithForm($petId, $name = null, $status = null)
    {
        // Implement the operation ...
    }

    // ...
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **int**| ID of pet that needs to be updated |
 **name** | **string**| Updated name of the pet | [optional]
 **status** | **string**| Updated status of the pet | [optional]

### Return type

void (empty response body)

### Authorization

[petstore_auth](../../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: application/x-www-form-urlencoded
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

## **uploadFile**
> OpenAPI\Server\Model\ApiResponse uploadFile($petId, $additionalMetadata, $file)

uploads an image

### Example Implementation
```php
<?php
// src/Acme/MyBundle/Api/PetApiInterface.php

namespace Acme\MyBundle\Api;

use OpenAPI\Server\Api\PetApiInterface;

class PetApi implements PetApiInterface
{

    /**
     * Configure OAuth2 access token for authorization: petstore_auth
     */
    public function setpetstore_auth($oauthToken)
    {
        // Retrieve logged in user from $oauthToken ...
    }

    // ...

    /**
     * Implementation of PetApiInterface#uploadFile
     */
    public function uploadFile($petId, $additionalMetadata = null, UploadedFile $file = null)
    {
        // Implement the operation ...
    }

    // ...
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **petId** | **int**| ID of pet to update |
 **additionalMetadata** | **string**| Additional data to pass to server | [optional]
 **file** | **UploadedFile****UploadedFile**| file to upload | [optional]

### Return type

[**OpenAPI\Server\Model\ApiResponse**](../Model/ApiResponse.md)

### Authorization

[petstore_auth](../../README.md#petstore_auth)

### HTTP request headers

 - **Content-Type**: multipart/form-data
 - **Accept**: application/json

[[Back to top]](#) [[Back to API list]](../../README.md#documentation-for-api-endpoints) [[Back to Model list]](../../README.md#documentation-for-models) [[Back to README]](../../README.md)

