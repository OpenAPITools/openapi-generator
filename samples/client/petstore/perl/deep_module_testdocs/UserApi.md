# Something::Deep::UserApi

- [create_user](#create_user): Create user
- [create_users_with_array_input](#create_users_with_array_input): Creates list of users with given input array
- [create_users_with_list_input](#create_users_with_list_input): Creates list of users with given input array
- [login_user](#login_user): Logs user into the system
- [logout_user](#logout_user): Logs out current logged in user session
- [get_user_by_name](#get_user_by_name): Get user by user name
- [update_user](#update_user): Updated user
- [delete_user](#delete_user): Delete user

## **create_user**

Create user

This can only be done by the logged in user.

### Sample 
```perl
my $api = Something::Deep::UserApi->new();
my $body = new Something::Deep::Object::User->new(); # [User] Created user object

eval { 
    my $result = $api->create_user(body => $body);
};
if ($@) {
    warn "Exception when calling create_user: $@\n";
}
```

### Parameters
Required | Name | Type | Description 
------------ | ------------- | ------------- | -------------
 No | body | User | Created user object

### Return type

void (empty response body)

### HTTP headers

Content-Type: Not defined
Accept: application/json, application/xml

### Authentication scheme




## **create_users_with_array_input**

Creates list of users with given input array



### Sample 
```perl
my $api = Something::Deep::UserApi->new();
my $body = new Something::Deep::Object::ARRAY[User]->new(); # [ARRAY[User]] List of user object

eval { 
    my $result = $api->create_users_with_array_input(body => $body);
};
if ($@) {
    warn "Exception when calling create_users_with_array_input: $@\n";
}
```

### Parameters
Required | Name | Type | Description 
------------ | ------------- | ------------- | -------------
 No | body | ARRAY[User] | List of user object

### Return type

void (empty response body)

### HTTP headers

Content-Type: Not defined
Accept: application/json, application/xml

### Authentication scheme




## **create_users_with_list_input**

Creates list of users with given input array



### Sample 
```perl
my $api = Something::Deep::UserApi->new();
my $body = new Something::Deep::Object::ARRAY[User]->new(); # [ARRAY[User]] List of user object

eval { 
    my $result = $api->create_users_with_list_input(body => $body);
};
if ($@) {
    warn "Exception when calling create_users_with_list_input: $@\n";
}
```

### Parameters
Required | Name | Type | Description 
------------ | ------------- | ------------- | -------------
 No | body | ARRAY[User] | List of user object

### Return type

void (empty response body)

### HTTP headers

Content-Type: Not defined
Accept: application/json, application/xml

### Authentication scheme




## **login_user**

Logs user into the system



### Sample 
```perl
my $api = Something::Deep::UserApi->new();
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
Required | Name | Type | Description 
------------ | ------------- | ------------- | -------------
 No | username | string | The user name for login
 No | password | string | The password for login in clear text

### Return type

string

### HTTP headers

Content-Type: Not defined
Accept: application/json, application/xml

### Authentication scheme




## **logout_user**

Logs out current logged in user session



### Sample 
```perl
my $api = Something::Deep::UserApi->new();

eval { 
    my $result = $api->logout_user();
};
if ($@) {
    warn "Exception when calling logout_user: $@\n";
}
```

### Parameters
Required | Name | Type | Description 
------------ | ------------- | ------------- | -------------

### Return type

void (empty response body)

### HTTP headers

Content-Type: Not defined
Accept: application/json, application/xml

### Authentication scheme




## **get_user_by_name**

Get user by user name



### Sample 
```perl
my $api = Something::Deep::UserApi->new();
my $username = 'username_example'; # [string] The name that needs to be fetched. Use user1 for testing.

eval { 
    my $result = $api->get_user_by_name(username => $username);
};
if ($@) {
    warn "Exception when calling get_user_by_name: $@\n";
}
```

### Parameters
Required | Name | Type | Description 
------------ | ------------- | ------------- | -------------
 Yes | username | string | The name that needs to be fetched. Use user1 for testing.

### Return type

User

### HTTP headers

Content-Type: Not defined
Accept: application/json, application/xml

### Authentication scheme




## **update_user**

Updated user

This can only be done by the logged in user.

### Sample 
```perl
my $api = Something::Deep::UserApi->new();
my $username = 'username_example'; # [string] name that need to be deleted
my $body = new Something::Deep::Object::User->new(); # [User] Updated user object

eval { 
    my $result = $api->update_user(username => $username, body => $body);
};
if ($@) {
    warn "Exception when calling update_user: $@\n";
}
```

### Parameters
Required | Name | Type | Description 
------------ | ------------- | ------------- | -------------
 Yes | username | string | name that need to be deleted
 No | body | User | Updated user object

### Return type

void (empty response body)

### HTTP headers

Content-Type: Not defined
Accept: application/json, application/xml

### Authentication scheme




## **delete_user**

Delete user

This can only be done by the logged in user.

### Sample 
```perl
my $api = Something::Deep::UserApi->new();
my $username = 'username_example'; # [string] The name that needs to be deleted

eval { 
    my $result = $api->delete_user(username => $username);
};
if ($@) {
    warn "Exception when calling delete_user: $@\n";
}
```

### Parameters
Required | Name | Type | Description 
------------ | ------------- | ------------- | -------------
 Yes | username | string | The name that needs to be deleted

### Return type

void (empty response body)

### HTTP headers

Content-Type: Not defined
Accept: application/json, application/xml

### Authentication scheme




1;
