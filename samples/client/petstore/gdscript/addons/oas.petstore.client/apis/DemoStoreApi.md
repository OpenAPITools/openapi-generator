<a name="__pageTop"></a>
# DemoStoreApi   { #DemoStoreApi }


All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**delete_order**](#delete_order) | **DELETE** `/store/order/{orderId}` | Delete purchase order by ID
[**get_inventory**](#get_inventory) | **GET** `/store/inventory` | Returns pet inventories by status
[**get_order_by_id**](#get_order_by_id) | **GET** `/store/order/{orderId}` | Find purchase order by ID
[**place_order**](#place_order) | **POST** `/store/order` | Place an order for a pet

# **delete_order**   { #delete_order }
<a name="delete_order"></a>

> `delete_order(orderId: String, on_success: Callable, on_failure: Callable)`

Delete purchase order by ID

For valid response try integer IDs with value < 1000. Anything above 1000 or nonintegers will generate API errors

### Example


```gdscript

# Customize configuration
var config := DemoApiConfig.new()
config.host = "localhost"
config.port = 8080
#config.tls_enabled = true
#config.trusted_chain = preload("res://my_cert_chain.crt")

# Instantiate the api
var api = DemoStoreApi.new(config)
# You can also provide your own HTTPClient, to re-use it across apis.
#var api = DemoStoreApi.new(config, client)


# Invoke an endpoint
api.delete_order(
	# orderId: String = ""   Eg: orderId_example
	# ID of the order that needs to be deleted
	orderId,
	# On Success
	func(response):
		prints("Success!", "delete_order", response)
		
		pass  # do things, make stuff
		,
	# On Error
	func(error):  # error is DemoApiError
		push_error(str(error))
		,
)

```

# **get_inventory**   { #get_inventory }
<a name="get_inventory"></a>

> `get_inventory( on_success: Callable, on_failure: Callable)`

Returns pet inventories by status

Returns a map of status codes to quantities

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
var api = DemoStoreApi.new(config)
# You can also provide your own HTTPClient, to re-use it across apis.
#var api = DemoStoreApi.new(config, client)


# Invoke an endpoint
api.get_inventory(
	# On Success
	func(response):  # response is DemoApiResponse
		prints("Success!", "get_inventory", response)
		assert(response.data is DemointegerModel)
		pass  # do things, make stuff
		,
	# On Error
	func(error):  # error is DemoApiError
		push_error(str(error))
		,
)

```

# **get_order_by_id**   { #get_order_by_id }
<a name="get_order_by_id"></a>

> `get_order_by_id(orderId: float, on_success: Callable, on_failure: Callable)`

Find purchase order by ID

For valid response try integer IDs with value <= 5 or > 10. Other values will generate exceptions

### Example


```gdscript

# Customize configuration
var config := DemoApiConfig.new()
config.host = "localhost"
config.port = 8080
#config.tls_enabled = true
#config.trusted_chain = preload("res://my_cert_chain.crt")

# Instantiate the api
var api = DemoStoreApi.new(config)
# You can also provide your own HTTPClient, to re-use it across apis.
#var api = DemoStoreApi.new(config, client)


# Invoke an endpoint
api.get_order_by_id(
	# orderId: float   Eg: 789
	# ID of pet that needs to be fetched
	orderId,
	# On Success
	func(response):  # response is DemoApiResponse
		prints("Success!", "get_order_by_id", response)
		assert(response.data is DemoOrderModel)
		pass  # do things, make stuff
		,
	# On Error
	func(error):  # error is DemoApiError
		push_error(str(error))
		,
)

```

# **place_order**   { #place_order }
<a name="place_order"></a>

> `place_order(demoOrderModel: DemoOrderModel, on_success: Callable, on_failure: Callable)`

Place an order for a pet



### Example


```gdscript

# Customize configuration
var config := DemoApiConfig.new()
config.host = "localhost"
config.port = 8080
#config.tls_enabled = true
#config.trusted_chain = preload("res://my_cert_chain.crt")

# Instantiate the api
var api = DemoStoreApi.new(config)
# You can also provide your own HTTPClient, to re-use it across apis.
#var api = DemoStoreApi.new(config, client)

var demoOrderModel = DemoOrderModel.new()
# … fill model demoOrderModel with data

# Invoke an endpoint
api.place_order(
	# demoOrderModel: DemoOrderModel
	# order placed for purchasing the pet
	demoOrderModel,
	# On Success
	func(response):  # response is DemoApiResponse
		prints("Success!", "place_order", response)
		assert(response.data is DemoOrderModel)
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

