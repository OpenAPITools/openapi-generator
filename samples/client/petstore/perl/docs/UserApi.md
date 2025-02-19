# WWW::OpenAPIClient::UserApi

## Load the API package
```perl
use WWW::OpenAPIClient::Object::UserApi;
```

All URIs are relative to *http://petstore.swagger.io:80/v2*

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
> create_user(user => $user)

Create user

This can only be done by the logged in user.

### Example
```perl
use Data::Dumper;
use WWW::OpenAPIClient::UserApi;
my $api_instance = WWW::OpenAPIClient::UserApi->new(
);

my $user = WWW::OpenAPIClient::Object::User->new(); # User | Created user object

eval {
    $api_instance->create_user(user => $user);
};
if ($@) {
    warn "Exception when calling UserApi->create_user: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **user** | [**User**](User.md)| Created user object | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **create_users_with_array_input**
> create_users_with_array_input(user => $user)

Creates list of users with given input array



### Example
```perl
use Data::Dumper;
use WWW::OpenAPIClient::UserApi;
my $api_instance = WWW::OpenAPIClient::UserApi->new(
);

my $user = [WWW::OpenAPIClient::Object::ARRAY[User]->new()]; # ARRAY[User] | List of user object

eval {
    $api_instance->create_users_with_array_input(user => $user);
};
if ($@) {
    warn "Exception when calling UserApi->create_users_with_array_input: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **user** | [**ARRAY[User]**](User.md)| List of user object | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **create_users_with_list_input**
> create_users_with_list_input(user => $user)

Creates list of users with given input array



### Example
```perl
use Data::Dumper;
use WWW::OpenAPIClient::UserApi;
my $api_instance = WWW::OpenAPIClient::UserApi->new(
);

my $user = [WWW::OpenAPIClient::Object::ARRAY[User]->new()]; # ARRAY[User] | List of user object

eval {
    $api_instance->create_users_with_list_input(user => $user);
};
if ($@) {
    warn "Exception when calling UserApi->create_users_with_list_input: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **user** | [**ARRAY[User]**](User.md)| List of user object | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **delete_user**
> delete_user(username => $username)

Delete user

This can only be done by the logged in user.

### Example
```perl
use Data::Dumper;
use WWW::OpenAPIClient::UserApi;
my $api_instance = WWW::OpenAPIClient::UserApi->new(
);

my $username = "username_example"; # string | The name that needs to be deleted

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

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **get_user_by_name**
> User get_user_by_name(username => $username)

Get user by user name



### Example
```perl
use Data::Dumper;
use WWW::OpenAPIClient::UserApi;
my $api_instance = WWW::OpenAPIClient::UserApi->new(
);

my $username = "username_example"; # string | The name that needs to be fetched. Use user1 for testing.

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

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **login_user**
> string login_user(username => $username, password => $password)

Logs user into the system



### Example
```perl
use Data::Dumper;
use WWW::OpenAPIClient::UserApi;
my $api_instance = WWW::OpenAPIClient::UserApi->new(
);

my $username = "username_example"; # string | The user name for login
my $password = "password_example"; # string | The password for login in clear text

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
 **username** | **string**| The user name for login | 
 **password** | **string**| The password for login in clear text | 

### Return type

**string**

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: application/xml, application/json

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **logout_user**
> logout_user()

Logs out current logged in user session



### Example
```perl
use Data::Dumper;
use WWW::OpenAPIClient::UserApi;
my $api_instance = WWW::OpenAPIClient::UserApi->new(
);


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

### HTTP request headers

 - **Content-Type**: Not defined
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **update_user**
> update_user(username => $username, user => $user)

Updated user

This can only be done by the logged in user.

### Example
```perl
use Data::Dumper;
use WWW::OpenAPIClient::UserApi;
my $api_instance = WWW::OpenAPIClient::UserApi->new(
);

my $username = "username_example"; # string | name that need to be deleted
my $user = WWW::OpenAPIClient::Object::User->new(); # User | Updated user object

eval {
    $api_instance->update_user(username => $username, user => $user);
};
if ($@) {
    warn "Exception when calling UserApi->update_user: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **string**| name that need to be deleted | 
 **user** | [**User**](User.md)| Updated user object | 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP request headers

 - **Content-Type**: application/json
 - **Accept**: Not defined

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

