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

# Customize configuration, if needed
var config := DemoApiConfig.new()
config.host = "localhost"
config.port = 8080

var api = DemoStoreApi.new()
api.config = config  # optionally


api.delete_order(
	# orderId: String   Eg: orderId_example
	# ID of the order that needs to be deleted
	orderId,
	# On Success
	func(result):
		prints("Success!", result)
		pass  # do things
		,
	# On Error
	func(error):  # error is DemoApiError
		printerr(str(error))
		pass  # do things
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

# Customize configuration, if needed
var config := DemoApiConfig.new()
config.host = "localhost"
config.port = 8080

var api = DemoStoreApi.new()
api.config = config  # optionally


api.get_inventory(
	# On Success
	func(result):  # result is Demointeger
		prints("Success!", result)
		pass  # do things
		,
	# On Error
	func(error):  # error is DemoApiError
		printerr(str(error))
		pass  # do things
		,
)

```

# **get_order_by_id**   { #get_order_by_id }
<a name="get_order_by_id"></a>

> `get_order_by_id(orderId: float, on_success: Callable, on_failure: Callable)`

Find purchase order by ID

For valid response try integer IDs with value <= 5 or > 10. Other values will generated exceptions

### Example


```gdscript

# Customize configuration, if needed
var config := DemoApiConfig.new()
config.host = "localhost"
config.port = 8080

var api = DemoStoreApi.new()
api.config = config  # optionally


api.get_order_by_id(
	# orderId: float   Eg: 789
	# ID of pet that needs to be fetched
	orderId,
	# On Success
	func(result):  # result is DemoOrder
		prints("Success!", result)
		pass  # do things
		,
	# On Error
	func(error):  # error is DemoApiError
		printerr(str(error))
		pass  # do things
		,
)

```

# **place_order**   { #place_order }
<a name="place_order"></a>

> `place_order(demoOrder: DemoOrder, on_success: Callable, on_failure: Callable)`

Place an order for a pet



### Example


```gdscript

# Customize configuration, if needed
var config := DemoApiConfig.new()
config.host = "localhost"
config.port = 8080

var api = DemoStoreApi.new()
api.config = config  # optionally

var demoOrder = DemoOrder.new()
# … fill model with data

api.place_order(
	# demoOrder: DemoOrder
	# order placed for purchasing the pet
	demoOrder,
	# On Success
	func(result):  # result is DemoOrder
		prints("Success!", result)
		pass  # do things
		,
	# On Error
	func(error):  # error is DemoApiError
		printerr(str(error))
		pass  # do things
		,
)

```


### Authorization

[api_key](../README.md#api_key), 
[petstore_auth](../README.md#petstore_auth)

[[Back to top]](#__pageTop) \
[[Back to API list]](../README.md#documentation-for-api-endpoints) \
[[Back to Model list]](../README.md#documentation-for-models) \
[[Back to README]](../README.md) \

