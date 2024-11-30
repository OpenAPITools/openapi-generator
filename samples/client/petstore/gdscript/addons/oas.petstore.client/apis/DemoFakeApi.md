<a name="__pageTop"></a>
# DemoFakeApi   { #DemoFakeApi }


All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**test_nullable_required_param**](#test_nullable_required_param) | **GET** `/fake/user/{username}` | To test nullable required parameters

# **test_nullable_required_param**   { #test_nullable_required_param }
<a name="test_nullable_required_param"></a>

> `test_nullable_required_param(username: String,dummyRequiredNullableParam: String,uPPERCASE = "", on_success: Callable, on_failure: Callable)`

To test nullable required parameters



### Example


```gdscript

# Customize configuration
var config := DemoApiConfig.new()
config.host = "localhost"
config.port = 8080
#config.tls_enabled = true
#config.trusted_chain = preload("res://my_cert_chain.crt")

# Instantiate the api
var api = DemoFakeApi.new(config)
# You can also provide your own HTTPClient, to re-use it across apis.
#var api = DemoFakeApi.new(config, client)


# Invoke an endpoint
api.test_nullable_required_param(
	# username: String = ""   Eg: username_example
	# The name that needs to be fetched. Use user1 for testing.
	username,
	# dummyRequiredNullableParam: String = ""   Eg: dummyRequiredNullableParam_example
	# To test nullable required parameters
	dummyRequiredNullableParam,
	# uPPERCASE: String = ""   Eg: uPPERCASE_example
	# To test parameter names in upper case
	uPPERCASE,
	# On Success
	func(response):
		prints("Success!", "test_nullable_required_param", response)
		
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

