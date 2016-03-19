# WWW::SwaggerClient::UserApi

## Load the API package
```perl
use WWW::SwaggerClient::Object::UserApi;
```

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
> create_user(body => $body)

Create user

This can only be done by the logged in user.

### Example 
```perl
use Data::Dumper;

my $api_instance = WWW::SwaggerClient::UserApi->new();
my $body = WWW::SwaggerClient::Object::User->new(); # User | Created user object

eval { 
    $api_instance->create_user(body => $body);
};
if ($@) {
    warn "Exception when calling UserApi->create_user: $@\n";
}
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
> create_users_with_array_input(body => $body)

Creates list of users with given input array



### Example 
```perl
use Data::Dumper;

my $api_instance = WWW::SwaggerClient::UserApi->new();
my $body = (WWW::SwaggerClient::Object::ARRAY[User]->new()); # ARRAY[User] | List of user object

eval { 
    $api_instance->create_users_with_array_input(body => $body);
};
if ($@) {
    warn "Exception when calling UserApi->create_users_with_array_input: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**ARRAY[User]**](User.md)| List of user object | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **create_users_with_list_input**
> create_users_with_list_input(body => $body)

Creates list of users with given input array



### Example 
```perl
use Data::Dumper;

my $api_instance = WWW::SwaggerClient::UserApi->new();
my $body = (WWW::SwaggerClient::Object::ARRAY[User]->new()); # ARRAY[User] | List of user object

eval { 
    $api_instance->create_users_with_list_input(body => $body);
};
if ($@) {
    warn "Exception when calling UserApi->create_users_with_list_input: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**ARRAY[User]**](User.md)| List of user object | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **delete_user**
> delete_user(username => $username)

Delete user

This can only be done by the logged in user.

### Example 
```perl
use Data::Dumper;

# Configure HTTP basic authorization: test_http_basic
$WWW::SwaggerClient::Configuration::username = 'YOUR_USERNAME';
$WWW::SwaggerClient::Configuration::password = 'YOUR_PASSWORD';

my $api_instance = WWW::SwaggerClient::UserApi->new();
my $username = 'username_example'; # string | The name that needs to be deleted

eval { 
    $api_instance->delete_user(username => $username);
};
if ($@) {
    warn "Exception when calling UserApi->delete_user: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **string**| The name that needs to be deleted | 

### Return type

void (empty response body)

### Authorization

[test_http_basic](../README.md#test_http_basic)

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **get_user_by_name**
> User get_user_by_name(username => $username)

Get user by user name



### Example 
```perl
use Data::Dumper;

my $api_instance = WWW::SwaggerClient::UserApi->new();
my $username = 'username_example'; # string | The name that needs to be fetched. Use user1 for testing.

eval { 
    my $result = $api_instance->get_user_by_name(username => $username);
    print Dumper($result);
};
if ($@) {
    warn "Exception when calling UserApi->get_user_by_name: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **string**| The name that needs to be fetched. Use user1 for testing. | 

### Return type

[**User**](User.md)

### Authorization

No authorization required

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **login_user**
> string login_user(username => $username, password => $password)

Logs user into the system



### Example 
```perl
use Data::Dumper;

my $api_instance = WWW::SwaggerClient::UserApi->new();
my $username = 'username_example'; # string | The user name for login
my $password = 'password_example'; # string | The password for login in clear text

eval { 
    my $result = $api_instance->login_user(username => $username, password => $password);
    print Dumper($result);
};
if ($@) {
    warn "Exception when calling UserApi->login_user: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **string**| The user name for login | [optional] 
 **password** | **string**| The password for login in clear text | [optional] 

### Return type

**string**

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
```perl
use Data::Dumper;

my $api_instance = WWW::SwaggerClient::UserApi->new();

eval { 
    $api_instance->logout_user();
};
if ($@) {
    warn "Exception when calling UserApi->logout_user: $@\n";
}
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
> update_user(username => $username, body => $body)

Updated user

This can only be done by the logged in user.

### Example 
```perl
use Data::Dumper;

my $api_instance = WWW::SwaggerClient::UserApi->new();
my $username = 'username_example'; # string | name that need to be deleted
my $body = WWW::SwaggerClient::Object::User->new(); # User | Updated user object

eval { 
    $api_instance->update_user(username => $username, body => $body);
};
if ($@) {
    warn "Exception when calling UserApi->update_user: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **string**| name that need to be deleted | 
 **body** | [**User**](User.md)| Updated user object | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

