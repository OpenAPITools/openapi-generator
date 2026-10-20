<a name="__pageTop"></a>
# DemoTestingApi   { #DemoTestingApi }


All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**tests_file_response_get**](#tests_file_response_get) | **GET** `/tests/fileResponse` | Returns an image file
[**tests_type_testing_get**](#tests_type_testing_get) | **GET** `/tests/typeTesting` | Route to test the TypeTesting schema

# **tests_file_response_get**   { #tests_file_response_get }
<a name="tests_file_response_get"></a>

> `tests_file_response_get( on_success: Callable, on_failure: Callable)`

Returns an image file



### Example


```gdscript

# Customize configuration
var config := DemoApiConfig.new()
config.host = "localhost"
config.port = 8080
#config.tls_enabled = true
#config.trusted_chain = preload("res://my_cert_chain.crt")

# Instantiate the api
var api = DemoTestingApi.new(config)
# You can also provide your own HTTPClient, to re-use it across apis.
#var api = DemoTestingApi.new(config, client)


# Invoke an endpoint
api.tests_file_response_get(
	# On Success
	func(response):  # response is DemoApiResponse
		prints("Success!", "tests_file_response_get", response)
		assert(response.data is DemofileModel)
		pass  # do things, make stuff
		,
	# On Error
	func(error):  # error is DemoApiError
		push_error(str(error))
		,
)

```

# **tests_type_testing_get**   { #tests_type_testing_get }
<a name="tests_type_testing_get"></a>

> `tests_type_testing_get( on_success: Callable, on_failure: Callable)`

Route to test the TypeTesting schema



### Example


```gdscript

# Customize configuration
var config := DemoApiConfig.new()
config.host = "localhost"
config.port = 8080
#config.tls_enabled = true
#config.trusted_chain = preload("res://my_cert_chain.crt")

# Instantiate the api
var api = DemoTestingApi.new(config)
# You can also provide your own HTTPClient, to re-use it across apis.
#var api = DemoTestingApi.new(config, client)


# Invoke an endpoint
api.tests_type_testing_get(
	# On Success
	func(response):  # response is DemoApiResponse
		prints("Success!", "tests_type_testing_get", response)
		assert(response.data is DemoTypeTestingModel)
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

