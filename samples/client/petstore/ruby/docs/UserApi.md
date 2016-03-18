# Petstore::UserApi

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
> create_user(opts)

Create user

This can only be done by the logged in user.

### Example
```ruby
# load the gem
require 'petstore'

api_instance = Petstore::UserApi.new

opts = { 
  body: Petstore::User.new # User | Created user object
}

begin
  #Create user
  api_instance.create_user(opts)
rescue Petstore::ApiError => e
  puts "Exception when calling UserApi->create_user: #{e}"
end
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**User**](User.md)| Created user object | [optional] 

### Return type

nil (empty response body)

### Authorization

No authorization required

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml



# **create_users_with_array_input**
> create_users_with_array_input(opts)

Creates list of users with given input array



### Example
```ruby
# load the gem
require 'petstore'

api_instance = Petstore::UserApi.new

opts = { 
  body: [Petstore::User.new] # Array<User> | List of user object
}

begin
  #Creates list of users with given input array
  api_instance.create_users_with_array_input(opts)
rescue Petstore::ApiError => e
  puts "Exception when calling UserApi->create_users_with_array_input: #{e}"
end
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Array&lt;User&gt;**](User.md)| List of user object | [optional] 

### Return type

nil (empty response body)

### Authorization

No authorization required

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml



# **create_users_with_list_input**
> create_users_with_list_input(opts)

Creates list of users with given input array



### Example
```ruby
# load the gem
require 'petstore'

api_instance = Petstore::UserApi.new

opts = { 
  body: [Petstore::User.new] # Array<User> | List of user object
}

begin
  #Creates list of users with given input array
  api_instance.create_users_with_list_input(opts)
rescue Petstore::ApiError => e
  puts "Exception when calling UserApi->create_users_with_list_input: #{e}"
end
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Array&lt;User&gt;**](User.md)| List of user object | [optional] 

### Return type

nil (empty response body)

### Authorization

No authorization required

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml



# **delete_user**
> delete_user(username)

Delete user

This can only be done by the logged in user.

### Example
```ruby
# load the gem
require 'petstore'
# setup authorization 
Petstore.configure do |config|
  # Configure HTTP basic authorization: test_http_basic
  config.username = 'YOUR USERNAME'
  config.password = 'YOUR PASSWORD'
end

api_instance = Petstore::UserApi.new

username = "username_example" # String | The name that needs to be deleted


begin
  #Delete user
  api_instance.delete_user(username)
rescue Petstore::ApiError => e
  puts "Exception when calling UserApi->delete_user: #{e}"
end
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **String**| The name that needs to be deleted | 

### Return type

nil (empty response body)

### Authorization

[test_http_basic](../README.md#test_http_basic)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml



# **get_user_by_name**
> User get_user_by_name(username)

Get user by user name



### Example
```ruby
# load the gem
require 'petstore'

api_instance = Petstore::UserApi.new

username = "username_example" # String | The name that needs to be fetched. Use user1 for testing.


begin
  #Get user by user name
  result = api_instance.get_user_by_name(username)
  p result
rescue Petstore::ApiError => e
  puts "Exception when calling UserApi->get_user_by_name: #{e}"
end
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **String**| The name that needs to be fetched. Use user1 for testing. | 

### Return type

[**User**](User.md)

### Authorization

No authorization required

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml



# **login_user**
> String login_user(opts)

Logs user into the system



### Example
```ruby
# load the gem
require 'petstore'

api_instance = Petstore::UserApi.new

opts = { 
  username: "username_example", # String | The user name for login
  password: "password_example" # String | The password for login in clear text
}

begin
  #Logs user into the system
  result = api_instance.login_user(opts)
  p result
rescue Petstore::ApiError => e
  puts "Exception when calling UserApi->login_user: #{e}"
end
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **String**| The user name for login | [optional] 
 **password** | **String**| The password for login in clear text | [optional] 

### Return type

**String**

### Authorization

No authorization required

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml



# **logout_user**
> logout_user

Logs out current logged in user session



### Example
```ruby
# load the gem
require 'petstore'

api_instance = Petstore::UserApi.new

begin
  #Logs out current logged in user session
  api_instance.logout_user
rescue Petstore::ApiError => e
  puts "Exception when calling UserApi->logout_user: #{e}"
end
```

### Parameters
This endpoint does not need any parameter.

### Return type

nil (empty response body)

### Authorization

No authorization required

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml



# **update_user**
> update_user(username, opts)

Updated user

This can only be done by the logged in user.

### Example
```ruby
# load the gem
require 'petstore'

api_instance = Petstore::UserApi.new

username = "username_example" # String | name that need to be deleted

opts = { 
  body: Petstore::User.new # User | Updated user object
}

begin
  #Updated user
  api_instance.update_user(username, opts)
rescue Petstore::ApiError => e
  puts "Exception when calling UserApi->update_user: #{e}"
end
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **String**| name that need to be deleted | 
 **body** | [**User**](User.md)| Updated user object | [optional] 

### Return type

nil (empty response body)

### Authorization

No authorization required

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml



