<a name="__pageTop"></a>
# DemoPetApi   { #DemoPetApi }


All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**add_pet**](#add_pet) | **POST** `/pet` | Add a new pet to the store
[**delete_pet**](#delete_pet) | **DELETE** `/pet/{petId}` | Deletes a pet
[**find_pets_by_status**](#find_pets_by_status) | **GET** `/pet/findByStatus` | Finds Pets by status
[**find_pets_by_tags**](#find_pets_by_tags) | **GET** `/pet/findByTags` | Finds Pets by tags
[**get_pet_by_id**](#get_pet_by_id) | **GET** `/pet/{petId}` | Find pet by ID
[**update_pet**](#update_pet) | **PUT** `/pet` | Update an existing pet
[**update_pet_with_form**](#update_pet_with_form) | **POST** `/pet/{petId}` | Updates a pet in the store with form data
[**upload_file**](#upload_file) | **POST** `/pet/{petId}/uploadImage` | uploads an image

# **add_pet**   { #add_pet }
<a name="add_pet"></a>

> `add_pet(demoPetModel: DemoPetModel, on_success: Callable, on_failure: Callable)`

Add a new pet to the store



### Example

* OAuth Authentication (`petstore_auth`)

```gdscript

# Customize configuration
var config := DemoApiConfig.new()
config.host = "localhost"
config.port = 8080
#config.tls_enabled = true
#config.trusted_chain = preload("res://my_cert_chain.crt")

# Instantiate the api
var api = DemoPetApi.new(config)
# You can also provide your own HTTPClient, to re-use it across apis.
#var api = DemoPetApi.new(config, client)

var demoPetModel = DemoPetModel.new()
# … fill model demoPetModel with data

# Invoke an endpoint
api.add_pet(
	# demoPetModel: DemoPetModel
	# Pet object that needs to be added to the store
	demoPetModel,
	# On Success
	func(response):  # response is DemoApiResponse
		prints("Success!", "add_pet", response)
		assert(response.data is DemoPetModel)
		pass  # do things, make stuff
		,
	# On Error
	func(error):  # error is DemoApiError
		push_error(str(error))
		,
)

```

# **delete_pet**   { #delete_pet }
<a name="delete_pet"></a>

> `delete_pet(petId: float,apiKey = "", on_success: Callable, on_failure: Callable)`

Deletes a pet



### Example

* OAuth Authentication (`petstore_auth`)

```gdscript

# Customize configuration
var config := DemoApiConfig.new()
config.host = "localhost"
config.port = 8080
#config.tls_enabled = true
#config.trusted_chain = preload("res://my_cert_chain.crt")

# Instantiate the api
var api = DemoPetApi.new(config)
# You can also provide your own HTTPClient, to re-use it across apis.
#var api = DemoPetApi.new(config, client)


# Invoke an endpoint
api.delete_pet(
	# petId: float   Eg: 789
	# Pet id to delete
	petId,
	# apiKey: String = ""   Eg: apiKey_example
	apiKey,
	# On Success
	func(response):
		prints("Success!", "delete_pet", response)
		
		pass  # do things, make stuff
		,
	# On Error
	func(error):  # error is DemoApiError
		push_error(str(error))
		,
)

```

# **find_pets_by_status**   { #find_pets_by_status }
<a name="find_pets_by_status"></a>

> `find_pets_by_status(status: Array, on_success: Callable, on_failure: Callable)`

Finds Pets by status

Multiple status values can be provided with comma separated strings

### Example

* OAuth Authentication (`petstore_auth`)

```gdscript

# Customize configuration
var config := DemoApiConfig.new()
config.host = "localhost"
config.port = 8080
#config.tls_enabled = true
#config.trusted_chain = preload("res://my_cert_chain.crt")

# Instantiate the api
var api = DemoPetApi.new(config)
# You can also provide your own HTTPClient, to re-use it across apis.
#var api = DemoPetApi.new(config, client)


# Invoke an endpoint
api.find_pets_by_status(
	# status: Array
	# Status values that need to be considered for filter
	status,
	# On Success
	func(response):  # response is DemoApiResponse
		prints("Success!", "find_pets_by_status", response)
		assert(response.data is DemoPetModel)
		pass  # do things, make stuff
		,
	# On Error
	func(error):  # error is DemoApiError
		push_error(str(error))
		,
)

```

# **find_pets_by_tags**   { #find_pets_by_tags }
<a name="find_pets_by_tags"></a>

> `find_pets_by_tags(tags: Array, on_success: Callable, on_failure: Callable)`

Finds Pets by tags

Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.

### Example

* OAuth Authentication (`petstore_auth`)

```gdscript

# Customize configuration
var config := DemoApiConfig.new()
config.host = "localhost"
config.port = 8080
#config.tls_enabled = true
#config.trusted_chain = preload("res://my_cert_chain.crt")

# Instantiate the api
var api = DemoPetApi.new(config)
# You can also provide your own HTTPClient, to re-use it across apis.
#var api = DemoPetApi.new(config, client)


# Invoke an endpoint
api.find_pets_by_tags(
	# tags: Array
	# Tags to filter by
	tags,
	# On Success
	func(response):  # response is DemoApiResponse
		prints("Success!", "find_pets_by_tags", response)
		assert(response.data is DemoPetModel)
		pass  # do things, make stuff
		,
	# On Error
	func(error):  # error is DemoApiError
		push_error(str(error))
		,
)

```

# **get_pet_by_id**   { #get_pet_by_id }
<a name="get_pet_by_id"></a>

> `get_pet_by_id(petId: float, on_success: Callable, on_failure: Callable)`

Find pet by ID

Returns a single pet

### Example

* Api Key Authentication (`api_key`)

```gdscript

# Customize configuration
var config := DemoApiConfig.new()
config.host = "localhost"
config.port = 8080
#config.tls_enabled = true
#config.trusted_chain = preload("res://my_cert_chain.crt")

# Instantiate the api
var api = DemoPetApi.new(config)
# You can also provide your own HTTPClient, to re-use it across apis.
#var api = DemoPetApi.new(config, client)


# Invoke an endpoint
api.get_pet_by_id(
	# petId: float   Eg: 789
	# ID of pet to return
	petId,
	# On Success
	func(response):  # response is DemoApiResponse
		prints("Success!", "get_pet_by_id", response)
		assert(response.data is DemoPetModel)
		pass  # do things, make stuff
		,
	# On Error
	func(error):  # error is DemoApiError
		push_error(str(error))
		,
)

```

# **update_pet**   { #update_pet }
<a name="update_pet"></a>

> `update_pet(demoPetModel: DemoPetModel, on_success: Callable, on_failure: Callable)`

Update an existing pet



### Example

* OAuth Authentication (`petstore_auth`)

```gdscript

# Customize configuration
var config := DemoApiConfig.new()
config.host = "localhost"
config.port = 8080
#config.tls_enabled = true
#config.trusted_chain = preload("res://my_cert_chain.crt")

# Instantiate the api
var api = DemoPetApi.new(config)
# You can also provide your own HTTPClient, to re-use it across apis.
#var api = DemoPetApi.new(config, client)

var demoPetModel = DemoPetModel.new()
# … fill model demoPetModel with data

# Invoke an endpoint
api.update_pet(
	# demoPetModel: DemoPetModel
	# Pet object that needs to be added to the store
	demoPetModel,
	# On Success
	func(response):  # response is DemoApiResponse
		prints("Success!", "update_pet", response)
		assert(response.data is DemoPetModel)
		pass  # do things, make stuff
		,
	# On Error
	func(error):  # error is DemoApiError
		push_error(str(error))
		,
)

```

# **update_pet_with_form**   { #update_pet_with_form }
<a name="update_pet_with_form"></a>

> `update_pet_with_form(petId: float,name = "",status = "", on_success: Callable, on_failure: Callable)`

Updates a pet in the store with form data



### Example

* OAuth Authentication (`petstore_auth`)

```gdscript

# Customize configuration
var config := DemoApiConfig.new()
config.host = "localhost"
config.port = 8080
#config.tls_enabled = true
#config.trusted_chain = preload("res://my_cert_chain.crt")

# Instantiate the api
var api = DemoPetApi.new(config)
# You can also provide your own HTTPClient, to re-use it across apis.
#var api = DemoPetApi.new(config, client)


# Invoke an endpoint
api.update_pet_with_form(
	# petId: float   Eg: 789
	# ID of pet that needs to be updated
	petId,
	# name: String = ""   Eg: name_example
	# Updated name of the pet
	name,
	# status: String = ""   Eg: status_example
	# Updated status of the pet
	status,
	# On Success
	func(response):
		prints("Success!", "update_pet_with_form", response)
		
		pass  # do things, make stuff
		,
	# On Error
	func(error):  # error is DemoApiError
		push_error(str(error))
		,
)

```

# **upload_file**   { #upload_file }
<a name="upload_file"></a>

> `upload_file(petId: float,additionalMetadata = "",file = null, on_success: Callable, on_failure: Callable)`

uploads an image



### Example

* OAuth Authentication (`petstore_auth`)

```gdscript

# Customize configuration
var config := DemoApiConfig.new()
config.host = "localhost"
config.port = 8080
#config.tls_enabled = true
#config.trusted_chain = preload("res://my_cert_chain.crt")

# Instantiate the api
var api = DemoPetApi.new(config)
# You can also provide your own HTTPClient, to re-use it across apis.
#var api = DemoPetApi.new(config, client)


# Invoke an endpoint
api.upload_file(
	# petId: float   Eg: 789
	# ID of pet to update
	petId,
	# additionalMetadata: String = ""   Eg: additionalMetadata_example
	# Additional data to pass to server
	additionalMetadata,
	# file: String   Eg: BINARY_DATA_HERE
	# file to upload
	file,
	# On Success
	func(response):  # response is DemoApiResponse
		prints("Success!", "upload_file", response)
		assert(response.data is DemoApiResponseModel)
		pass  # do things, make stuff
		,
	# On Error
	func(error):  # error is DemoApiError
		push_error(str(error))
		,
)

```


### Authorization

[petstore_auth](../README.md#petstore_auth), 
[api_key](../README.md#api_key)

[[Back to top]](#__pageTop) \
[[Back to API list]](../README.md#documentation-for-api-endpoints) \
[[Back to Model list]](../README.md#documentation-for-models) \
[[Back to README]](../README.md) \

