# Documentation for OpenAPI Petstore
This is a client generator for microcontrollers on the Espressif32 platform and the Arduino framework
After the client have been generated, you have to change these following variablies:
- root.cert | Provide your service root certificate.
- src/main.cpp | Change wifi name
- src/main.cpp | Change wifi password
- lib/service/AbstractService.h | Change to your url

# Documentation for OpenAPI Petstore 1.0.0 Tiny client cpp (Arduino) 

The project is structured like this:
```
samples/client/petstore/tiny/cpp/
├── lib
│   ├── Models
│   ├── service
│   └── TestFiles
├── platformio.ini
├── pre_compiling_bourne.py
├── README.md
├── root.cert
├── src
│   └── main.cpp
└── test
    └── RunTests.cpp
```

All URIs are relative to http://petstore.swagger.iohttp://petstore.swagger.io/v2

### PetApi
|Method | HTTP request | Description|
|------------- | ------------- | -------------|
|*addPet* | *POST* /pet | Add a new pet to the store.|
|*deletePet* | *DELETE* /pet/{petId} | Deletes a pet.|
|*findPetsByStatus* | *GET* /pet/findByStatus | Finds Pets by status.|
|*findPetsByTags* | *GET* /pet/findByTags | Finds Pets by tags.|
|*getPetById* | *GET* /pet/{petId} | Find pet by ID.|
|*updatePet* | *PUT* /pet | Update an existing pet.|
|*updatePetWithForm* | *POST* /pet/{petId} | Updates a pet in the store with form data.|
|*uploadFile* | *POST* /pet/{petId}/uploadImage | uploads an image.|

### StoreApi
|Method | HTTP request | Description|
|------------- | ------------- | -------------|
|*deleteOrder* | *DELETE* /store/order/{orderId} | Delete purchase order by ID.|
|*getInventory* | *GET* /store/inventory | Returns pet inventories by status.|
|*getOrderById* | *GET* /store/order/{orderId} | Find purchase order by ID.|
|*placeOrder* | *POST* /store/order | Place an order for a pet.|

### UserApi
|Method | HTTP request | Description|
|------------- | ------------- | -------------|
|*createUser* | *POST* /user | Create user.|
|*createUsersWithArrayInput* | *POST* /user/createWithArray | Creates list of users with given input array.|
|*createUsersWithListInput* | *POST* /user/createWithList | Creates list of users with given input array.|
|*deleteUser* | *DELETE* /user/{username} | Delete user.|
|*getUserByName* | *GET* /user/{username} | Get user by user name.|
|*loginUser* | *GET* /user/login | Logs user into the system.|
|*logoutUser* | *GET* /user/logout | Logs out current logged in user session.|
|*updateUser* | *PUT* /user/{username} | Updated user.|


## What are the Model files for the data structures/objects?
|Class | Description|
|------------- | -------------|
|*ApiResponse* | Describes the result of uploading an image resource|
|*Category* | A category for a pet|
|*Order* | An order for a pets from the pet store|
|*Pet* | A pet for sale in the pet store|
|*Tag* | A tag for a pet|
|*User* | A User who is purchasing from the pet store|


