# OpenAPI Petstore

This is a sample server Petstore server. For this sample, you can use the api key `special-key` to test the authorization filters.


## API

### Pet

|Name|Role|
|----|----|
|`api.PetController`|Play Framework API controller|
|`api.PetApi`|Representing trait|
|`api.PetApiImpl`|Default implementation|

* `POST /v2/pet` - Add a new pet to the store
* `DELETE /v2/pet/:petId` - Deletes a pet
* `GET /v2/pet/findByStatus?status=[value]` - Finds Pets by status
* `GET /v2/pet/findByTags?tags=[value]` - Finds Pets by tags
* `GET /v2/pet/:petId` - Find pet by ID
* `PUT /v2/pet` - Update an existing pet
* `POST /v2/pet/:petId` - Updates a pet in the store with form data
* `POST /v2/pet/:petId/uploadImage` - uploads an image

### Store

|Name|Role|
|----|----|
|`api.StoreController`|Play Framework API controller|
|`api.StoreApi`|Representing trait|
|`api.StoreApiImpl`|Default implementation|

* `DELETE /v2/store/order/:orderId` - Delete purchase order by ID
* `GET /v2/store/inventory` - Returns pet inventories by status
* `GET /v2/store/order/:orderId` - Find purchase order by ID
* `POST /v2/store/order` - Place an order for a pet

### User

|Name|Role|
|----|----|
|`api.UserController`|Play Framework API controller|
|`api.UserApi`|Representing trait|
|`api.UserApiImpl`|Default implementation|

* `POST /v2/user` - Create user
* `POST /v2/user/createWithArray` - Creates list of users with given input array
* `POST /v2/user/createWithList` - Creates list of users with given input array
* `DELETE /v2/user/:username` - Delete user
* `GET /v2/user/:username` - Get user by user name
* `GET /v2/user/login?username=[value]&password=[value]` - Logs user into the system
* `GET /v2/user/logout` - Logs out current logged in user session
* `PUT /v2/user/:username` - Updated user

