# swagger-petstore-okhttp-gson

## Requirements

Building the API client library requires [Maven](https://maven.apache.org/) to be installed.

## Installation

To install the API client library to your local Maven repository, simply execute:

```shell
mvn install
```

To deploy it to a remote Maven repository instead, configure the settings of the repository and execute:

```shell
mvn deploy
```

Refer to the [official documentation](https://maven.apache.org/plugins/maven-deploy-plugin/usage.html) for more information.

### Maven users

Add this dependency to your project's POM:

```xml
<dependency>
    <groupId>io.swagger</groupId>
    <artifactId>swagger-petstore-okhttp-gson</artifactId>
    <version>1.0.0</version>
    <scope>compile</scope>
</dependency>
```

### Gradle users

Add this dependency to your project's build file:

```groovy
compile "io.swagger:swagger-petstore-okhttp-gson:1.0.0"
```

### Others

At first generate the JAR by executing:

    mvn package

Then manually install the following JARs:

* target/swagger-petstore-okhttp-gson-1.0.0.jar
* target/lib/*.jar

## Getting Started

Please follow the [installation](#installation) instruction and execute the following Java code:

```java

import io.swagger.client.*;
import io.swagger.client.auth.*;
import io.swagger.client.model.*;
import io.swagger.client.api.FakeApi;

import java.io.File;
import java.util.*;

public class FakeApiExample {

    public static void main(String[] args) {
        
        FakeApi apiInstance = new FakeApi();
        BigDecimal number = new BigDecimal(); // BigDecimal | None
        Double _double = 3.4D; // Double | None
        String string = "string_example"; // String | None
        byte[] _byte = B; // byte[] | None
        Integer integer = 56; // Integer | None
        Integer int32 = 56; // Integer | None
        Long int64 = 789L; // Long | None
        Float _float = 3.4F; // Float | None
        byte[] binary = B; // byte[] | None
        LocalDate date = new LocalDate(); // LocalDate | None
        DateTime dateTime = new DateTime(); // DateTime | None
        String password = "password_example"; // String | None
        try {
            apiInstance.testEndpointParameters(number, _double, string, _byte, integer, int32, int64, _float, binary, date, dateTime, password);
        } catch (ApiException e) {
            System.err.println("Exception when calling FakeApi#testEndpointParameters");
            e.printStackTrace();
        }
    }
}

```

## Documentation for API Endpoints

All URIs are relative to *http://petstore.swagger.io/v2*

Class | Method | HTTP request | Description
------------ | ------------- | ------------- | -------------
*FakeApi* | [**testEndpointParameters**](docs/FakeApi.md#testEndpointParameters) | **POST** /fake | Fake endpoint for testing various parameters 假端點 偽のエンドポイント 가짜 엔드 포인트 
*FakeApi* | [**testEnumQueryParameters**](docs/FakeApi.md#testEnumQueryParameters) | **GET** /fake | To test enum query parameters
*PetApi* | [**addPet**](docs/PetApi.md#addPet) | **POST** /pet | Add a new pet to the store
*PetApi* | [**deletePet**](docs/PetApi.md#deletePet) | **DELETE** /pet/{petId} | Deletes a pet
*PetApi* | [**findPetsByStatus**](docs/PetApi.md#findPetsByStatus) | **GET** /pet/findByStatus | Finds Pets by status
*PetApi* | [**findPetsByTags**](docs/PetApi.md#findPetsByTags) | **GET** /pet/findByTags | Finds Pets by tags
*PetApi* | [**getPetById**](docs/PetApi.md#getPetById) | **GET** /pet/{petId} | Find pet by ID
*PetApi* | [**updatePet**](docs/PetApi.md#updatePet) | **PUT** /pet | Update an existing pet
*PetApi* | [**updatePetWithForm**](docs/PetApi.md#updatePetWithForm) | **POST** /pet/{petId} | Updates a pet in the store with form data
*PetApi* | [**uploadFile**](docs/PetApi.md#uploadFile) | **POST** /pet/{petId}/uploadImage | uploads an image
*StoreApi* | [**deleteOrder**](docs/StoreApi.md#deleteOrder) | **DELETE** /store/order/{orderId} | Delete purchase order by ID
*StoreApi* | [**getInventory**](docs/StoreApi.md#getInventory) | **GET** /store/inventory | Returns pet inventories by status
*StoreApi* | [**getOrderById**](docs/StoreApi.md#getOrderById) | **GET** /store/order/{orderId} | Find purchase order by ID
*StoreApi* | [**placeOrder**](docs/StoreApi.md#placeOrder) | **POST** /store/order | Place an order for a pet
*UserApi* | [**createUser**](docs/UserApi.md#createUser) | **POST** /user | Create user
*UserApi* | [**createUsersWithArrayInput**](docs/UserApi.md#createUsersWithArrayInput) | **POST** /user/createWithArray | Creates list of users with given input array
*UserApi* | [**createUsersWithListInput**](docs/UserApi.md#createUsersWithListInput) | **POST** /user/createWithList | Creates list of users with given input array
*UserApi* | [**deleteUser**](docs/UserApi.md#deleteUser) | **DELETE** /user/{username} | Delete user
*UserApi* | [**getUserByName**](docs/UserApi.md#getUserByName) | **GET** /user/{username} | Get user by user name
*UserApi* | [**loginUser**](docs/UserApi.md#loginUser) | **GET** /user/login | Logs user into the system
*UserApi* | [**logoutUser**](docs/UserApi.md#logoutUser) | **GET** /user/logout | Logs out current logged in user session
*UserApi* | [**updateUser**](docs/UserApi.md#updateUser) | **PUT** /user/{username} | Updated user


## Documentation for Models

 - [AdditionalPropertiesClass](docs/AdditionalPropertiesClass.md)
 - [Animal](docs/Animal.md)
 - [AnimalFarm](docs/AnimalFarm.md)
 - [ArrayOfArrayOfNumberOnly](docs/ArrayOfArrayOfNumberOnly.md)
 - [ArrayOfNumberOnly](docs/ArrayOfNumberOnly.md)
 - [ArrayTest](docs/ArrayTest.md)
 - [Cat](docs/Cat.md)
 - [Category](docs/Category.md)
 - [Dog](docs/Dog.md)
 - [EnumClass](docs/EnumClass.md)
 - [EnumTest](docs/EnumTest.md)
 - [FormatTest](docs/FormatTest.md)
 - [HasOnlyReadOnly](docs/HasOnlyReadOnly.md)
 - [MapTest](docs/MapTest.md)
 - [MixedPropertiesAndAdditionalPropertiesClass](docs/MixedPropertiesAndAdditionalPropertiesClass.md)
 - [Model200Response](docs/Model200Response.md)
 - [ModelApiResponse](docs/ModelApiResponse.md)
 - [ModelReturn](docs/ModelReturn.md)
 - [Name](docs/Name.md)
 - [NumberOnly](docs/NumberOnly.md)
 - [Order](docs/Order.md)
 - [Pet](docs/Pet.md)
 - [ReadOnlyFirst](docs/ReadOnlyFirst.md)
 - [SpecialModelName](docs/SpecialModelName.md)
 - [Tag](docs/Tag.md)
 - [User](docs/User.md)


## Documentation for Authorization

Authentication schemes defined for the API:
### api_key

- **Type**: API key
- **API key parameter name**: api_key
- **Location**: HTTP header

### petstore_auth

- **Type**: OAuth
- **Flow**: implicit
- **Authorizatoin URL**: http://petstore.swagger.io/api/oauth/dialog
- **Scopes**: 
  - write:pets: modify pets in your account
  - read:pets: read your pets


## Recommendation

It's recommended to create an instance of `ApiClient` per thread in a multithreaded environment to avoid any potential issue.

## Author

apiteam@swagger.io

