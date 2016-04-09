# swagger_client\UserApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**create_user**](UserApi.md#create_user) | **POST** /user | Create user
[**create_users_with_array_input**](UserApi.md#create_users_with_array_input) | **POST** /user/createWithArray | Creates list of users with given input array
[**create_users_with_list_input**](UserApi.md#create_users_with_list_input) | **POST** /user/createWithList | Creates list of users with given input array
[**delete_user**](UserApi.md#delete_user) | **DELETE** /user/{username} | Delete user
[**get_user_by_name**](UserApi.md#get_user_by_name) | **GET** /user/{username} | Get user by user name
[**login_user**](UserApi.md#login_user) | **GET** /user/login | Logs user into the system
[**logout_user**](UserApi.md#logout_user) | **GET** /user/logout | Logs out current logged in user session
[**update_user**](UserApi.md#update_user) | **PUT** /user/{username} | Updated user


# **create_user**
> create_user(body=body)

Create user

This can only be done by the logged in user.

### Example 
```python
import swagger_client
from swagger_client.rest import ApiException
from pprint import pprint
import time


# create an instance of the API class
api_instance = swagger_client.UserApi()
body = swagger_client.User() # User | Created user object (optional)

try: 
    # Create user
    api_instance.create_user(body=body);
except ApiException as e:
    print "Exception when calling UserApi->create_user: %s\n" % e
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**User**](User.md)| Created user object | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **create_users_with_array_input**
> create_users_with_array_input(body=body)

Creates list of users with given input array



### Example 
```python
import swagger_client
from swagger_client.rest import ApiException
from pprint import pprint
import time


# create an instance of the API class
api_instance = swagger_client.UserApi()
body = [swagger_client.User()] # list[User] | List of user object (optional)

try: 
    # Creates list of users with given input array
    api_instance.create_users_with_array_input(body=body);
except ApiException as e:
    print "Exception when calling UserApi->create_users_with_array_input: %s\n" % e
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**list[User]**](User.md)| List of user object | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **create_users_with_list_input**
> create_users_with_list_input(body=body)

Creates list of users with given input array



### Example 
```python
import swagger_client
from swagger_client.rest import ApiException
from pprint import pprint
import time


# create an instance of the API class
api_instance = swagger_client.UserApi()
body = [swagger_client.User()] # list[User] | List of user object (optional)

try: 
    # Creates list of users with given input array
    api_instance.create_users_with_list_input(body=body);
except ApiException as e:
    print "Exception when calling UserApi->create_users_with_list_input: %s\n" % e
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**list[User]**](User.md)| List of user object | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **delete_user**
> delete_user(username)

Delete user

This can only be done by the logged in user.

### Example 
```python
import swagger_client
from swagger_client.rest import ApiException
from pprint import pprint
import time


# Configure HTTP basic authorization: test_http_basic
swagger_client.configuration.username = 'YOUR_USERNAME'
swagger_client.configuration.password = 'YOUR_PASSWORD'

# create an instance of the API class
api_instance = swagger_client.UserApi()
username = 'username_example' # str | The name that needs to be deleted

try: 
    # Delete user
    api_instance.delete_user(username);
except ApiException as e:
    print "Exception when calling UserApi->delete_user: %s\n" % e
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **str**| The name that needs to be deleted | 

### Return type

void (empty response body)

### Authorization

[test_http_basic](../README.md#test_http_basic)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **get_user_by_name**
> User get_user_by_name(username)

Get user by user name



### Example 
```python
import swagger_client
from swagger_client.rest import ApiException
from pprint import pprint
import time


# create an instance of the API class
api_instance = swagger_client.UserApi()
username = 'username_example' # str | The name that needs to be fetched. Use user1 for testing.

try: 
    # Get user by user name
    api_response = api_instance.get_user_by_name(username);
    pprint(api_response)
except ApiException as e:
    print "Exception when calling UserApi->get_user_by_name: %s\n" % e
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **str**| The name that needs to be fetched. Use user1 for testing. | 

### Return type

[**User**](User.md)

### Authorization

No authorization required

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **login_user**
> str login_user(username=username, password=password)

Logs user into the system



### Example 
```python
import swagger_client
from swagger_client.rest import ApiException
from pprint import pprint
import time


# create an instance of the API class
api_instance = swagger_client.UserApi()
username = 'username_example' # str | The user name for login (optional)
password = 'password_example' # str | The password for login in clear text (optional)

try: 
    # Logs user into the system
    api_response = api_instance.login_user(username=username, password=password);
    pprint(api_response)
except ApiException as e:
    print "Exception when calling UserApi->login_user: %s\n" % e
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **str**| The user name for login | [optional] 
 **password** | **str**| The password for login in clear text | [optional] 

### Return type

**str**

### Authorization

No authorization required

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **logout_user**
> logout_user()

Logs out current logged in user session



### Example 
```python
import swagger_client
from swagger_client.rest import ApiException
from pprint import pprint
import time


# create an instance of the API class
api_instance = swagger_client.UserApi()

try: 
    # Logs out current logged in user session
    api_instance.logout_user();
except ApiException as e:
    print "Exception when calling UserApi->logout_user: %s\n" % e
```

### Parameters
This endpoint does not need any parameter.

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **update_user**
> update_user(username, body=body)

Updated user

This can only be done by the logged in user.

### Example 
```python
import swagger_client
from swagger_client.rest import ApiException
from pprint import pprint
import time


# create an instance of the API class
api_instance = swagger_client.UserApi()
username = 'username_example' # str | name that need to be deleted
body = swagger_client.User() # User | Updated user object (optional)

try: 
    # Updated user
    api_instance.update_user(username, body=body);
except ApiException as e:
    print "Exception when calling UserApi->update_user: %s\n" % e
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **str**| name that need to be deleted | 
 **body** | [**User**](User.md)| Updated user object | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

