# Documentation for OpenAPI Petstore 1.0.0 Tizen Client SDK

## How do I get the doc files?
First generate source code by running `openapi-generator`
Then run `doc/generateDocumentation.sh` from the output folder. It will generate all the doc files and put them in the `doc/SDK` directory.
To successfully generate documentation it needs `Doxygen` installed in the path.
*Note* - Before generating the documentation, put the logo of the project as the file `doc/logo.png` before running `doxygen`.


## How do I use this?
This is the structure of the doc folder:

```
.
├── logo.png                  \\Logo of the project
├── Doxyfile                  \\Doxygen config files
├── generateDocumentation.sh  \\Script to run to generate documentation
├── README.md                 \\This file
├── SDK                       \\Documentation for all classes in OpenAPI Petstore Tizen Client SDK. See ./html/index.html
│   └── html

```

## *tl;dr* run this:

```
doc/generateDocumentation.sh
```

The above SDK folder will be generated. See the index.html inside the SDK folder.


## What's Doxygen?
Doxygen is the de facto standard tool for generating/extracting documentation from annotated/unannotated C++ sources, but it also supports other popular programming languages such as C, Objective-C, C#, PHP, Java, Python, IDL (Corba, Microsoft, and UNO/OpenOffice flavors), Fortran, VHDL, Tcl, and to some extent D.

Check out [Doxygen](https://www.doxygen.org/) for additional information about the Doxygen project.

## I Don't want to run Doxygen. What are the API files for accessing the REST endpoints?
All URIs are relative to http://petstore.swagger.iohttp://petstore.swagger.io/v2

### PetManager
Method | HTTP request | Description
------------- | ------------- | -------------
*addPetSync* | *POST* /pet | Add a new pet to the store.
*addPetASync* | *POST* /pet | Add a new pet to the store.
*deletePetSync* | *DELETE* /pet/{petId} | Deletes a pet.
*deletePetASync* | *DELETE* /pet/{petId} | Deletes a pet.
*findPetsByStatusSync* | *GET* /pet/findByStatus | Finds Pets by status.
*findPetsByStatusASync* | *GET* /pet/findByStatus | Finds Pets by status.
*findPetsByTagsSync* | *GET* /pet/findByTags | Finds Pets by tags.
*findPetsByTagsASync* | *GET* /pet/findByTags | Finds Pets by tags.
*getPetByIdSync* | *GET* /pet/{petId} | Find pet by ID.
*getPetByIdASync* | *GET* /pet/{petId} | Find pet by ID.
*updatePetSync* | *PUT* /pet | Update an existing pet.
*updatePetASync* | *PUT* /pet | Update an existing pet.
*updatePetWithFormSync* | *POST* /pet/{petId} | Updates a pet in the store with form data.
*updatePetWithFormASync* | *POST* /pet/{petId} | Updates a pet in the store with form data.
*uploadFileSync* | *POST* /pet/{petId}/uploadImage | uploads an image.
*uploadFileASync* | *POST* /pet/{petId}/uploadImage | uploads an image.

### StoreManager
Method | HTTP request | Description
------------- | ------------- | -------------
*deleteOrderSync* | *DELETE* /store/order/{orderId} | Delete purchase order by ID.
*deleteOrderASync* | *DELETE* /store/order/{orderId} | Delete purchase order by ID.
*getInventorySync* | *GET* /store/inventory | Returns pet inventories by status.
*getInventoryASync* | *GET* /store/inventory | Returns pet inventories by status.
*getOrderByIdSync* | *GET* /store/order/{orderId} | Find purchase order by ID.
*getOrderByIdASync* | *GET* /store/order/{orderId} | Find purchase order by ID.
*placeOrderSync* | *POST* /store/order | Place an order for a pet.
*placeOrderASync* | *POST* /store/order | Place an order for a pet.

### UserManager
Method | HTTP request | Description
------------- | ------------- | -------------
*createUserSync* | *POST* /user | Create user.
*createUserASync* | *POST* /user | Create user.
*createUsersWithArrayInputSync* | *POST* /user/createWithArray | Creates list of users with given input array.
*createUsersWithArrayInputASync* | *POST* /user/createWithArray | Creates list of users with given input array.
*createUsersWithListInputSync* | *POST* /user/createWithList | Creates list of users with given input array.
*createUsersWithListInputASync* | *POST* /user/createWithList | Creates list of users with given input array.
*deleteUserSync* | *DELETE* /user/{username} | Delete user.
*deleteUserASync* | *DELETE* /user/{username} | Delete user.
*getUserByNameSync* | *GET* /user/{username} | Get user by user name.
*getUserByNameASync* | *GET* /user/{username} | Get user by user name.
*loginUserSync* | *GET* /user/login | Logs user into the system.
*loginUserASync* | *GET* /user/login | Logs user into the system.
*logoutUserSync* | *GET* /user/logout | Logs out current logged in user session.
*logoutUserASync* | *GET* /user/logout | Logs out current logged in user session.
*updateUserSync* | *PUT* /user/{username} | Updated user.
*updateUserASync* | *PUT* /user/{username} | Updated user.


## What are the Model files for the data structures/objects?
Class | Description
------------- | -------------
 *ApiResponse* | Describes the result of uploading an image resource
 *Category* | A category for a pet
 *Order* | An order for a pets from the pet store
 *Pet* | A pet for sale in the pet store
 *Tag* | A tag for a pet
 *User* | A User who is purchasing from the pet store

