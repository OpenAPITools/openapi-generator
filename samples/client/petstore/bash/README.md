# Swagger Petstore Bash client

## Overview
This is a Bash client script for accessing Swagger Petstore service.

The script uses cURL underneath for making all REST calls.

## Usage

```shell
# Make sure the script has executable rights
$ chmod u+x petstore-cli

# Print the list of operations available on the service
$ ./petstore-cli -h

# Print the service description
$ ./petstore-cli --about

# Print detailed information about specific operation
$ ./petstore-cli <operationId> -h

# Make GET request
./petstore-cli --host http://<hostname>:<port> --accept xml <operationId> <queryParam1>=<value1> <header_key1>:<header_value2>

# Make GET request using arbitrary curl options (must be passed before <operationId>) to an SSL service using username:password
petstore-cli -k -sS --tlsv1.2 --host https://<hostname> -u <user>:<password> --accept xml <operationId> <queryParam1>=<value1> <header_key1>:<header_value2>

# Make POST request
$ echo '<body_content>' | petstore-cli --host <hostname> --content-type json <operationId> -

# Make POST request with simple JSON content, e.g.:
# {
#   "key1": "value1",
#   "key2": "value2",
#   "key3": 23
# }
$ echo '<body_content>' | petstore-cli --host <hostname> --content-type json <operationId> key1==value1 key2=value2 key3:=23 -

# Preview the cURL command without actually executing it
$ petstore-cli --host http://<hostname>:<port> --dry-run <operationid>

```

## Docker image
You can easily create a Docker image containing a preconfigured environment
for using the REST Bash client including working autocompletion and short
welcome message with basic instructions, using the generated Dockerfile:

```shell
docker build -t my-rest-client .
docker run -it my-rest-client
```

By default you will be logged into a Zsh environment which has much more
advanced auto completion, but you can switch to Bash, where basic autocompletion
is also available.

## Shell completion

### Bash
The generated bash-completion script can be either directly loaded to the current Bash session using:

```shell
source petstore-cli.bash-completion
```

Alternatively, the script can be copied to the `/etc/bash-completion.d` (or on OSX with Homebrew to `/usr/local/etc/bash-completion.d`):

```shell
sudo cp petstore-cli.bash-completion /etc/bash-completion.d/petstore-cli
```

#### OS X
On OSX you might need to install bash-completion using Homebrew:
```shell
brew install bash-completion
```
and add the following to the `~/.bashrc`:

```shell
if [ -f $(brew --prefix)/etc/bash_completion ]; then
  . $(brew --prefix)/etc/bash_completion
fi
```

### Zsh
In Zsh, the generated `_petstore-cli` Zsh completion file must be copied to one of the folders under `$FPATH` variable.


## Documentation for API Endpoints

All URIs are relative to */v2*

Class | Method | HTTP request | Description
------------ | ------------- | ------------- | -------------
*AnotherFakeApi* | [**testSpecialTags**](docs/AnotherFakeApi.md#testspecialtags) | **PATCH** /another-fake/dummy | To test special tags
*FakeApi* | [**fakeOuterBooleanSerialize**](docs/FakeApi.md#fakeouterbooleanserialize) | **POST** /fake/outer/boolean | 
*FakeApi* | [**fakeOuterCompositeSerialize**](docs/FakeApi.md#fakeoutercompositeserialize) | **POST** /fake/outer/composite | 
*FakeApi* | [**fakeOuterNumberSerialize**](docs/FakeApi.md#fakeouternumberserialize) | **POST** /fake/outer/number | 
*FakeApi* | [**fakeOuterStringSerialize**](docs/FakeApi.md#fakeouterstringserialize) | **POST** /fake/outer/string | 
*FakeApi* | [**testClientModel**](docs/FakeApi.md#testclientmodel) | **PATCH** /fake | To test \&quot;client\&quot; model
*FakeApi* | [**testEndpointParameters**](docs/FakeApi.md#testendpointparameters) | **POST** /fake | Fake endpoint for testing various parameters
假端點
偽のエンドポイント
가짜 엔드 포인트
*FakeApi* | [**testEnumParameters**](docs/FakeApi.md#testenumparameters) | **GET** /fake | To test enum parameters
*FakeApi* | [**testInlineAdditionalProperties**](docs/FakeApi.md#testinlineadditionalproperties) | **POST** /fake/inline-additionalProperties | test inline additionalProperties
*FakeApi* | [**testJsonFormData**](docs/FakeApi.md#testjsonformdata) | **GET** /fake/jsonFormData | test json serialization of form data
*FakeClassnameTags123Api* | [**testClassname**](docs/FakeClassnameTags123Api.md#testclassname) | **PATCH** /fake_classname_test | To test class name in snake case
*PetApi* | [**addPet**](docs/PetApi.md#addpet) | **POST** /pet | Add a new pet to the store
*PetApi* | [**deletePet**](docs/PetApi.md#deletepet) | **DELETE** /pet/{petId} | Deletes a pet
*PetApi* | [**findPetsByStatus**](docs/PetApi.md#findpetsbystatus) | **GET** /pet/findByStatus | Finds Pets by status
*PetApi* | [**findPetsByTags**](docs/PetApi.md#findpetsbytags) | **GET** /pet/findByTags | Finds Pets by tags
*PetApi* | [**getPetById**](docs/PetApi.md#getpetbyid) | **GET** /pet/{petId} | Find pet by ID
*PetApi* | [**updatePet**](docs/PetApi.md#updatepet) | **PUT** /pet | Update an existing pet
*PetApi* | [**updatePetWithForm**](docs/PetApi.md#updatepetwithform) | **POST** /pet/{petId} | Updates a pet in the store with form data
*PetApi* | [**uploadFile**](docs/PetApi.md#uploadfile) | **POST** /pet/{petId}/uploadImage | uploads an image
*StoreApi* | [**deleteOrder**](docs/StoreApi.md#deleteorder) | **DELETE** /store/order/{order_id} | Delete purchase order by ID
*StoreApi* | [**getInventory**](docs/StoreApi.md#getinventory) | **GET** /store/inventory | Returns pet inventories by status
*StoreApi* | [**getOrderById**](docs/StoreApi.md#getorderbyid) | **GET** /store/order/{order_id} | Find purchase order by ID
*StoreApi* | [**placeOrder**](docs/StoreApi.md#placeorder) | **POST** /store/order | Place an order for a pet
*UserApi* | [**createUser**](docs/UserApi.md#createuser) | **POST** /user | Create user
*UserApi* | [**createUsersWithArrayInput**](docs/UserApi.md#createuserswitharrayinput) | **POST** /user/createWithArray | Creates list of users with given input array
*UserApi* | [**createUsersWithListInput**](docs/UserApi.md#createuserswithlistinput) | **POST** /user/createWithList | Creates list of users with given input array
*UserApi* | [**deleteUser**](docs/UserApi.md#deleteuser) | **DELETE** /user/{username} | Delete user
*UserApi* | [**getUserByName**](docs/UserApi.md#getuserbyname) | **GET** /user/{username} | Get user by user name
*UserApi* | [**loginUser**](docs/UserApi.md#loginuser) | **GET** /user/login | Logs user into the system
*UserApi* | [**logoutUser**](docs/UserApi.md#logoutuser) | **GET** /user/logout | Logs out current logged in user session
*UserApi* | [**updateUser**](docs/UserApi.md#updateuser) | **PUT** /user/{username} | Updated user


## Documentation For Models

 - [$special[model.name]](docs/$special[model.name].md)
 - [200_response](docs/200_response.md)
 - [AdditionalPropertiesClass](docs/AdditionalPropertiesClass.md)
 - [Animal](docs/Animal.md)
 - [AnimalFarm](docs/AnimalFarm.md)
 - [ApiResponse](docs/ApiResponse.md)
 - [ArrayOfArrayOfNumberOnly](docs/ArrayOfArrayOfNumberOnly.md)
 - [ArrayOfNumberOnly](docs/ArrayOfNumberOnly.md)
 - [ArrayTest](docs/ArrayTest.md)
 - [Capitalization](docs/Capitalization.md)
 - [Category](docs/Category.md)
 - [ClassModel](docs/ClassModel.md)
 - [Client](docs/Client.md)
 - [EnumArrays](docs/EnumArrays.md)
 - [EnumClass](docs/EnumClass.md)
 - [Enum_Test](docs/Enum_Test.md)
 - [Format_test](docs/Format_test.md)
 - [HasOnlyReadOnly](docs/HasOnlyReadOnly.md)
 - [MapTest](docs/MapTest.md)
 - [MixedPropertiesAndAdditionalPropertiesClass](docs/MixedPropertiesAndAdditionalPropertiesClass.md)
 - [Name](docs/Name.md)
 - [NumberOnly](docs/NumberOnly.md)
 - [Order](docs/Order.md)
 - [OuterBoolean](docs/OuterBoolean.md)
 - [OuterComposite](docs/OuterComposite.md)
 - [OuterEnum](docs/OuterEnum.md)
 - [OuterNumber](docs/OuterNumber.md)
 - [OuterString](docs/OuterString.md)
 - [Pet](docs/Pet.md)
 - [ReadOnlyFirst](docs/ReadOnlyFirst.md)
 - [Return](docs/Return.md)
 - [Tag](docs/Tag.md)
 - [User](docs/User.md)
 - [Cat](docs/Cat.md)
 - [Dog](docs/Dog.md)


## Documentation For Authorization


## api_key

- **Type**: API key
- **API key parameter name**: api_key
- **Location**: HTTP header

## api_key_query

- **Type**: API key
- **API key parameter name**: api_key_query
- **Location**: URL query string

## http_basic_test

- **Type**: HTTP basic authentication

## petstore_auth

- **Type**: OAuth
- **Flow**: implicit
- **Authorization URL**: http://petstore.swagger.io/api/oauth/dialog
- **Scopes**:
  - **write:pets**: modify pets in your account
  - **read:pets**: read your pets

