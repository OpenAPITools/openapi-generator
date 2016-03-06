# WWW::SwaggerClient::UserApi

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**create_user**](UserApi.md#create_user) | **POST** /user | Create user
[**create_users_with_array_input**](UserApi.md#create_users_with_array_input) | **POST** /user/createWithArray | Creates list of users with given input array
[**create_users_with_list_input**](UserApi.md#create_users_with_list_input) | **POST** /user/createWithList | Creates list of users with given input array
[**login_user**](UserApi.md#login_user) | **GET** /user/login | Logs user into the system
[**logout_user**](UserApi.md#logout_user) | **GET** /user/logout | Logs out current logged in user session
[**get_user_by_name**](UserApi.md#get_user_by_name) | **GET** /user/{username} | Get user by user name
[**update_user**](UserApi.md#update_user) | **PUT** /user/{username} | Updated user
[**delete_user**](UserApi.md#delete_user) | **DELETE** /user/{username} | Delete user


# **create_user**
> create_user(body => $body)

Create user

This can only be done by the logged in user.

### Sample 
```perl
my $api = WWW::SwaggerClient::UserApi->new();
my $body = WWW::SwaggerClient::Object::User->new(); # [User] Created user object

eval { 
    my $result = $api->create_user(body => $body);
};
if ($@) {
    warn "Exception when calling create_user: $@\n";
}
```

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**User**](docs/.md)| Created user object | [optional] 

### Return type

void (empty response body)

### HTTP headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

### Authentication scheme

No authentiation required





# **create_users_with_array_input**
> create_users_with_array_input(body => $body)

Creates list of users with given input array



### Sample 
```perl
my $api = WWW::SwaggerClient::UserApi->new();
my $body = (WWW::SwaggerClient::Object::ARRAY[User]->new()); # [ARRAY[User]] List of user object

eval { 
    my $result = $api->create_users_with_array_input(body => $body);
};
if ($@) {
    warn "Exception when calling create_users_with_array_input: $@\n";
}
```

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**ARRAY[User]**](docs/.md)| List of user object | [optional] 

### Return type

void (empty response body)

### HTTP headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

### Authentication scheme

No authentiation required





# **create_users_with_list_input**
> create_users_with_list_input(body => $body)

Creates list of users with given input array



### Sample 
```perl
my $api = WWW::SwaggerClient::UserApi->new();
my $body = (WWW::SwaggerClient::Object::ARRAY[User]->new()); # [ARRAY[User]] List of user object

eval { 
    my $result = $api->create_users_with_list_input(body => $body);
};
if ($@) {
    warn "Exception when calling create_users_with_list_input: $@\n";
}
```

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**ARRAY[User]**](docs/.md)| List of user object | [optional] 

### Return type

void (empty response body)

### HTTP headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

### Authentication scheme

No authentiation required





# **login_user**
> login_user(username => $username, password => $password)

Logs user into the system



### Sample 
```perl
my $api = WWW::SwaggerClient::UserApi->new();
my $username = 'username_example'; # [string] The user name for login
my $password = 'password_example'; # [string] The password for login in clear text

eval { 
    my $result = $api->login_user(username => $username, password => $password);
};
if ($@) {
    warn "Exception when calling login_user: $@\n";
}
```

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | [**string**](docs/.md)| The user name for login | [optional] 
 **password** | [**string**](docs/.md)| The password for login in clear text | [optional] 

### Return type

[**string**](string.md)

### HTTP headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

### Authentication scheme

No authentiation required





# **logout_user**
> logout_user()

Logs out current logged in user session



### Sample 
```perl
my $api = WWW::SwaggerClient::UserApi->new();

eval { 
    my $result = $api->logout_user();
};
if ($@) {
    warn "Exception when calling logout_user: $@\n";
}
```

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------

### Return type

void (empty response body)

### HTTP headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

### Authentication scheme

No authentiation required





# **get_user_by_name**
> get_user_by_name(username => $username)

Get user by user name



### Sample 
```perl
my $api = WWW::SwaggerClient::UserApi->new();
my $username = 'username_example'; # [string] The name that needs to be fetched. Use user1 for testing.

eval { 
    my $result = $api->get_user_by_name(username => $username);
};
if ($@) {
    warn "Exception when calling get_user_by_name: $@\n";
}
```

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | [**string**](docs/.md)| The name that needs to be fetched. Use user1 for testing. | 

### Return type

[**User**](User.md)

### HTTP headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

### Authentication scheme

No authentiation required





# **update_user**
> update_user(username => $username, body => $body)

Updated user

This can only be done by the logged in user.

### Sample 
```perl
my $api = WWW::SwaggerClient::UserApi->new();
my $username = 'username_example'; # [string] name that need to be deleted
my $body = WWW::SwaggerClient::Object::User->new(); # [User] Updated user object

eval { 
    my $result = $api->update_user(username => $username, body => $body);
};
if ($@) {
    warn "Exception when calling update_user: $@\n";
}
```

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | [**string**](docs/.md)| name that need to be deleted | 
 **body** | [**User**](docs/.md)| Updated user object | [optional] 

### Return type

void (empty response body)

### HTTP headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

### Authentication scheme

No authentiation required





# **delete_user**
> delete_user(username => $username)

Delete user

This can only be done by the logged in user.

### Sample 
```perl
my $api = WWW::SwaggerClient::UserApi->new();
my $username = 'username_example'; # [string] The name that needs to be deleted

eval { 
    my $result = $api->delete_user(username => $username);
};
if ($@) {
    warn "Exception when calling delete_user: $@\n";
}
```

### Parameters
Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **username** | [**string**](docs/.md)| The name that needs to be deleted | 

### Return type

void (empty response body)

### HTTP headers

 - **Content-Type**: Not defined
 - **Accept**: application/json, application/xml

### Authentication scheme

No authentiation required





