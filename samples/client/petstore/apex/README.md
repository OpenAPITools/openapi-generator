# Swagger Petstore API Client

This is a sample server Petstore server.  You can find out more about Swagger at [http://swagger.io](http://swagger.io) or on [irc.freenode.net, #swagger](http://swagger.io/irc/).  For this sample, you can use the api key `special-key` to test the authorization filters.

## Requirements

- [Java 8 JDK](http://www.oracle.com/technetwork/java/javase/downloads/index.html)
- [Apache Ant](http://ant.apache.org/) version 1.6 or later
- [Force.com Migration Tool](https://developer.salesforce.com/docs/atlas.en-us.daas.meta/daas/forcemigrationtool_install.htm)
  - The `ant-salesforce.jar` file included with the Force.com Migration Tool must be placed in the root directory of this project (in the same directory as this README and `build.xml`)
- `ANT_HOME` and `JAVA_HOME` environment variables must be set accordingly
  - On Windows, `JAVA_HOME` will probably look something like this:

    ```
    JAVA_HOME = C:\Program Files\Java\jdk1.8.0_121
    ```

- The `bin` directory from Ant must be on your `PATH`

If everything is set correctly:

- Running `java -version` in a command prompt should output something like:

  ```bash
  java version "1.8.0_121"
  Java(TM) SE Runtime Environment (build 1.8.0_121-b13)
  Java HotSpot(TM) 64-Bit Server VM (build 25.121-b13, mixed mode)
  ```

- Running `ant -version` should output something like:

  ```bash
  Apache Ant(TM) version 1.10.1 compiled on February 2 2017
  ```

For more information, see <https://developer.salesforce.com/docs/atlas.en-us.daas.meta/daas/forcemigrationtool_prereq.htm>

## Installation


1. Clone the repo from GitHub

    ```bash
    $ git clone git@github.com:GIT_USER_ID/GIT_REPO_ID.git
    ```

    Or, [download](https://github.com/GIT_USER_ID/GIT_REPO_ID/archive/master.zip) the repo as a ZIP and extract it to `GIT_REPO_ID`

1. Set the `SF_USERNAME` and `SF_PASSWORD` environment variables to your Salesforce username and password. Alternatively, they may be set in `build.properties`. Environment variables will override the values in `build.properties` if set.

    `SF_SESSIONID` may also be set instead of `SF_USERNAME` and `SF_PASSWORD` (more info in `build.properties`)

2. Open up a command prompt in the root project directory `GIT_REPO_ID` (the same directory as this README and `build.xml`)
3. Deploy to your Salesforce org

    ```bash
    $ ant deploy
    ```

    This command will:

    - deploy all classes in the `deploy/classes` directory to your Salesforce org
    - create a new [unmanaged package](https://help.salesforce.com/articleView?id=sharing_apps.htm) called **Swagger Petstore API Client**
    - execute all of the unit tests included in this package (at least 75% code coverage required)
    - create a new [remote site](https://help.salesforce.com/articleView?id=configuring_remoteproxy.htm) called **Swagger_Petstore** configured for the endpoint: <http://petstore.swagger.io/v2>
    - rolls back any changes upon any error

    A successful deployment will look like this:

    ```bash
    [sf:deploy] Request Status: Succeeded
    [sf:deploy] *********** DEPLOYMENT SUCCEEDED ***********
    [sf:deploy] Finished request 0Af7A00000Ebd5oSAB successfully.

    BUILD SUCCESSFUL
    Total time: 34 seconds
    ```

### Test deployment only

To perform a test deployment without committing changes:

```bash
$ ant deployCheckOnly
```

All of the included tests will run as if it were a real deployment. Tests and other validations will report back while leaving your org untouched, allowing you to verify that a deployment will succeed without affecting anything in the org.

### Uninstallation

```bash
$ ant undeploy
```

Removes all classes and the Remote Site created by this package.

## Getting Started

Please follow the [installation](#installation) instruction and execute the following Apex code:

```java
SwagPetApi api = new SwagPetApi();
SwagClient client = api.getClient();

// Configure OAuth2 access token for authorization: petstore_auth
Swagger.OAuth petstore_auth = (Swagger.OAuth) client.getAuthentication('petstore_auth');
petstore_auth.setAccessToken('YOUR ACCESS TOKEN');

Map<String, Object> params = new Map<String, Object>{
    'body' => SwagPet.getExample()
};

try {
    // cross your fingers
    api.addPet(params);
} catch (Swagger.ApiException e) {
    // ...handle your exceptions
}
```

## Documentation for API Endpoints

All URIs are relative to *http://petstore.swagger.io/v2*

Class | Method | HTTP request | Description
------------ | ------------- | ------------- | -------------
*SwagPetApi* | [**addPet**](docs/SwagPetApi.md#addPet) | **POST** /pet | Add a new pet to the store
*SwagPetApi* | [**deletePet**](docs/SwagPetApi.md#deletePet) | **DELETE** /pet/{petId} | Deletes a pet
*SwagPetApi* | [**findPetsByStatus**](docs/SwagPetApi.md#findPetsByStatus) | **GET** /pet/findByStatus | Finds Pets by status
*SwagPetApi* | [**findPetsByTags**](docs/SwagPetApi.md#findPetsByTags) | **GET** /pet/findByTags | Finds Pets by tags
*SwagPetApi* | [**getPetById**](docs/SwagPetApi.md#getPetById) | **GET** /pet/{petId} | Find pet by ID
*SwagPetApi* | [**updatePet**](docs/SwagPetApi.md#updatePet) | **PUT** /pet | Update an existing pet
*SwagPetApi* | [**updatePetWithForm**](docs/SwagPetApi.md#updatePetWithForm) | **POST** /pet/{petId} | Updates a pet in the store with form data
*SwagPetApi* | [**uploadFile**](docs/SwagPetApi.md#uploadFile) | **POST** /pet/{petId}/uploadImage | uploads an image
*SwagStoreApi* | [**deleteOrder**](docs/SwagStoreApi.md#deleteOrder) | **DELETE** /store/order/{orderId} | Delete purchase order by ID
*SwagStoreApi* | [**getInventory**](docs/SwagStoreApi.md#getInventory) | **GET** /store/inventory | Returns pet inventories by status
*SwagStoreApi* | [**getOrderById**](docs/SwagStoreApi.md#getOrderById) | **GET** /store/order/{orderId} | Find purchase order by ID
*SwagStoreApi* | [**placeOrder**](docs/SwagStoreApi.md#placeOrder) | **POST** /store/order | Place an order for a pet
*SwagUserApi* | [**createUser**](docs/SwagUserApi.md#createUser) | **POST** /user | Create user
*SwagUserApi* | [**createUsersWithArrayInput**](docs/SwagUserApi.md#createUsersWithArrayInput) | **POST** /user/createWithArray | Creates list of users with given input array
*SwagUserApi* | [**createUsersWithListInput**](docs/SwagUserApi.md#createUsersWithListInput) | **POST** /user/createWithList | Creates list of users with given input array
*SwagUserApi* | [**deleteUser**](docs/SwagUserApi.md#deleteUser) | **DELETE** /user/{username} | Delete user
*SwagUserApi* | [**getUserByName**](docs/SwagUserApi.md#getUserByName) | **GET** /user/{username} | Get user by user name
*SwagUserApi* | [**loginUser**](docs/SwagUserApi.md#loginUser) | **GET** /user/login | Logs user into the system
*SwagUserApi* | [**logoutUser**](docs/SwagUserApi.md#logoutUser) | **GET** /user/logout | Logs out current logged in user session
*SwagUserApi* | [**updateUser**](docs/SwagUserApi.md#updateUser) | **PUT** /user/{username} | Updated user


## Documentation for Models

 - [SwagApiResponse](docs/SwagApiResponse.md)
 - [SwagCategory](docs/SwagCategory.md)
 - [SwagOrder](docs/SwagOrder.md)
 - [SwagPet](docs/SwagPet.md)
 - [SwagTag](docs/SwagTag.md)
 - [SwagUser](docs/SwagUser.md)


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


## Author

apiteam@swagger.io

