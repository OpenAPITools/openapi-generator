# ::UserApi

## Load the API package
```perl
use ::Object::UserApi;
```

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**createUser**](UserApi.md#createUser) | **POST** /user | Create user
[**createUsersWithArrayInput**](UserApi.md#createUsersWithArrayInput) | **POST** /user/createWithArray | Creates list of users with given input array
[**createUsersWithListInput**](UserApi.md#createUsersWithListInput) | **POST** /user/createWithList | Creates list of users with given input array
[**deleteUser**](UserApi.md#deleteUser) | **DELETE** /user/{username} | Delete user
[**getUserByName**](UserApi.md#getUserByName) | **GET** /user/{username} | Get user by user name
[**loginUser**](UserApi.md#loginUser) | **GET** /user/login | Logs user into the system
[**logoutUser**](UserApi.md#logoutUser) | **GET** /user/logout | Logs out current logged in user session
[**updateUser**](UserApi.md#updateUser) | **PUT** /user/{username} | Updated user


# **createUser**
> createUser(body => $body)

Create user

This can only be done by the logged in user.

### Example 
```perl
use Data::Dumper;

my $api = ::UserApi->new();
my $body = ::Object::\Swagger\Client\Model\User->new(); # [\Swagger\Client\Model\User] Created user object

eval { 
    $api->createUser(body => $body);
};
if ($@) {
    warn "Exception when calling UserApi->createUser: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**\Swagger\Client\Model\User**](\Swagger\Client\Model\User.md)| Created user object | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **createUsersWithArrayInput**
> createUsersWithArrayInput(body => $body)

Creates list of users with given input array



### Example 
```perl
use Data::Dumper;

my $api = ::UserApi->new();
my $body = (::Object::\Swagger\Client\Model\User[]->new()); # [\Swagger\Client\Model\User[]] List of user object

eval { 
    $api->createUsersWithArrayInput(body => $body);
};
if ($@) {
    warn "Exception when calling UserApi->createUsersWithArrayInput: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**\Swagger\Client\Model\User[]**](User.md)| List of user object | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **createUsersWithListInput**
> createUsersWithListInput(body => $body)

Creates list of users with given input array



### Example 
```perl
use Data::Dumper;

my $api = ::UserApi->new();
my $body = (::Object::\Swagger\Client\Model\User[]->new()); # [\Swagger\Client\Model\User[]] List of user object

eval { 
    $api->createUsersWithListInput(body => $body);
};
if ($@) {
    warn "Exception when calling UserApi->createUsersWithListInput: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**\Swagger\Client\Model\User[]**](User.md)| List of user object | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **deleteUser**
> deleteUser(username => $username)

Delete user

This can only be done by the logged in user.

### Example 
```perl
use Data::Dumper;

# Configure HTTP basic authorization: test_http_basic
::Configuration::username = 'YOUR_USERNAME';
::Configuration::password = 'YOUR_PASSWORD';

my $api = ::UserApi->new();
my $username = username_example; # [string] The name that needs to be deleted

eval { 
    $api->deleteUser(username => $username);
};
if ($@) {
    warn "Exception when calling UserApi->deleteUser: $@\n";
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

# **getUserByName**
> \Swagger\Client\Model\User getUserByName(username => $username)

Get user by user name



### Example 
```perl
use Data::Dumper;

my $api = ::UserApi->new();
my $username = username_example; # [string] The name that needs to be fetched. Use user1 for testing.

eval { 
    my $result = $api->getUserByName(username => $username);
    print Dumper($result);
};
if ($@) {
    warn "Exception when calling UserApi->getUserByName: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **string**| The name that needs to be fetched. Use user1 for testing. | 

### Return type

[**\Swagger\Client\Model\User**](User.md)

### Authorization

No authorization required

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

# **loginUser**
> string loginUser(username => $username, password => $password)

Logs user into the system



### Example 
```perl
use Data::Dumper;

my $api = ::UserApi->new();
my $username = username_example; # [string] The user name for login
my $password = password_example; # [string] The password for login in clear text

eval { 
    my $result = $api->loginUser(username => $username, password => $password);
    print Dumper($result);
};
if ($@) {
    warn "Exception when calling UserApi->loginUser: $@\n";
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

# **logoutUser**
> logoutUser()

Logs out current logged in user session



### Example 
```perl
use Data::Dumper;

my $api = ::UserApi->new();

eval { 
    $api->logoutUser();
};
if ($@) {
    warn "Exception when calling UserApi->logoutUser: $@\n";
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

# **updateUser**
> updateUser(username => $username, body => $body)

Updated user

This can only be done by the logged in user.

### Example 
```perl
use Data::Dumper;

my $api = ::UserApi->new();
my $username = username_example; # [string] name that need to be deleted
my $body = ::Object::\Swagger\Client\Model\User->new(); # [\Swagger\Client\Model\User] Updated user object

eval { 
    $api->updateUser(username => $username, body => $body);
};
if ($@) {
    warn "Exception when calling UserApi->updateUser: $@\n";
}
```

### Parameters

Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | **string**| name that need to be deleted | 
 **body** | [**\Swagger\Client\Model\User**](\Swagger\Client\Model\User.md)| Updated user object | [optional] 

### Return type

void (empty response body)

### Authorization

No authorization required

### HTTP reuqest headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

[[Back to top]](#) [[Back to API list]](../README.md#documentation-for-api-endpoints) [[Back to Model list]](../README.md#documentation-for-models) [[Back to README]](../README.md)

