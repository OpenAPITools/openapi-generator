# Something::Deep::PetApi

- [update_pet](#update_pet): Update an existing pet
- [add_pet](#add_pet): Add a new pet to the store
- [find_pets_by_status](#find_pets_by_status): Finds Pets by status
- [find_pets_by_tags](#find_pets_by_tags): Finds Pets by tags
- [get_pet_by_id](#get_pet_by_id): Find pet by ID
- [update_pet_with_form](#update_pet_with_form): Updates a pet in the store with form data
- [delete_pet](#delete_pet): Deletes a pet
- [upload_file](#upload_file): uploads an image
- [get_pet_by_id_in_object](#get_pet_by_id_in_object): Fake endpoint to test inline arbitrary object return by &#39;Find pet by ID&#39;
- [pet_pet_idtesting_byte_arraytrue_get](#pet_pet_idtesting_byte_arraytrue_get): Fake endpoint to test byte array return by &#39;Find pet by ID&#39;
- [add_pet_using_byte_array](#add_pet_using_byte_array): Fake endpoint to test byte array in body parameter for adding a new pet to the store

## **update_pet**

Update an existing pet



### Sample 
```perl
my $api = Something::Deep::PetApi->new();
my $body = new Something::Deep::Object::Pet->new(); # [Pet] Pet object that needs to be added to the store

eval { 
    my $result = $api->update_pet(body => $body);
};
if ($@) {
    warn "Exception when calling update_pet: $@\n";
}
```

### Parameters
Required | Name | Type | Description 
------------ | ------------- | ------------- | -------------
 No | body | Pet | Pet object that needs to be added to the store

### Return type

void (empty response body)

### HTTP headers

Content-Type: application/json, application/xml
Accept: application/json, application/xml

### Authentication scheme

petstore_auth 


## **add_pet**

Add a new pet to the store



### Sample 
```perl
my $api = Something::Deep::PetApi->new();
my $body = new Something::Deep::Object::Pet->new(); # [Pet] Pet object that needs to be added to the store

eval { 
    my $result = $api->add_pet(body => $body);
};
if ($@) {
    warn "Exception when calling add_pet: $@\n";
}
```

### Parameters
Required | Name | Type | Description 
------------ | ------------- | ------------- | -------------
 No | body | Pet | Pet object that needs to be added to the store

### Return type

void (empty response body)

### HTTP headers

Content-Type: application/json, application/xml
Accept: application/json, application/xml

### Authentication scheme

petstore_auth 


## **find_pets_by_status**

Finds Pets by status

Multiple status values can be provided with comma separated strings

### Sample 
```perl
my $api = Something::Deep::PetApi->new();
my $status = ; # [ARRAY[string]] Status values that need to be considered for query

eval { 
    my $result = $api->find_pets_by_status(status => $status);
};
if ($@) {
    warn "Exception when calling find_pets_by_status: $@\n";
}
```

### Parameters
Required | Name | Type | Description 
------------ | ------------- | ------------- | -------------
 No | status | ARRAY[string] | Status values that need to be considered for query

### Return type

ARRAY[Pet]

### HTTP headers

Content-Type: Not defined
Accept: application/json, application/xml

### Authentication scheme

petstore_auth 


## **find_pets_by_tags**

Finds Pets by tags

Muliple tags can be provided with comma seperated strings. Use tag1, tag2, tag3 for testing.

### Sample 
```perl
my $api = Something::Deep::PetApi->new();
my $tags = ; # [ARRAY[string]] Tags to filter by

eval { 
    my $result = $api->find_pets_by_tags(tags => $tags);
};
if ($@) {
    warn "Exception when calling find_pets_by_tags: $@\n";
}
```

### Parameters
Required | Name | Type | Description 
------------ | ------------- | ------------- | -------------
 No | tags | ARRAY[string] | Tags to filter by

### Return type

ARRAY[Pet]

### HTTP headers

Content-Type: Not defined
Accept: application/json, application/xml

### Authentication scheme

petstore_auth 


## **get_pet_by_id**

Find pet by ID

Returns a pet when ID < 10.  ID > 10 or nonintegers will simulate API error conditions

### Sample 
```perl
my $api = Something::Deep::PetApi->new();
my $pet_id = 789; # [int] ID of pet that needs to be fetched

eval { 
    my $result = $api->get_pet_by_id(pet_id => $pet_id);
};
if ($@) {
    warn "Exception when calling get_pet_by_id: $@\n";
}
```

### Parameters
Required | Name | Type | Description 
------------ | ------------- | ------------- | -------------
 Yes | pet_id | int | ID of pet that needs to be fetched

### Return type

Pet

### HTTP headers

Content-Type: Not defined
Accept: application/json, application/xml

### Authentication scheme

api_key petstore_auth 


## **update_pet_with_form**

Updates a pet in the store with form data



### Sample 
```perl
my $api = Something::Deep::PetApi->new();
my $pet_id = 'pet_id_example'; # [string] ID of pet that needs to be updated
my $name = 'name_example'; # [string] Updated name of the pet
my $status = 'status_example'; # [string] Updated status of the pet

eval { 
    my $result = $api->update_pet_with_form(pet_id => $pet_id, name => $name, status => $status);
};
if ($@) {
    warn "Exception when calling update_pet_with_form: $@\n";
}
```

### Parameters
Required | Name | Type | Description 
------------ | ------------- | ------------- | -------------
 Yes | pet_id | string | ID of pet that needs to be updated
 No | name | string | Updated name of the pet
 No | status | string | Updated status of the pet

### Return type

void (empty response body)

### HTTP headers

Content-Type: application/x-www-form-urlencoded
Accept: application/json, application/xml

### Authentication scheme

petstore_auth 


## **delete_pet**

Deletes a pet



### Sample 
```perl
my $api = Something::Deep::PetApi->new();
my $pet_id = 789; # [int] Pet id to delete
my $api_key = 'api_key_example'; # [string] 

eval { 
    my $result = $api->delete_pet(pet_id => $pet_id, api_key => $api_key);
};
if ($@) {
    warn "Exception when calling delete_pet: $@\n";
}
```

### Parameters
Required | Name | Type | Description 
------------ | ------------- | ------------- | -------------
 Yes | pet_id | int | Pet id to delete
 No | api_key | string | 

### Return type

void (empty response body)

### HTTP headers

Content-Type: Not defined
Accept: application/json, application/xml

### Authentication scheme

petstore_auth 


## **upload_file**

uploads an image



### Sample 
```perl
my $api = Something::Deep::PetApi->new();
my $pet_id = 789; # [int] ID of pet to update
my $additional_metadata = 'additional_metadata_example'; # [string] Additional data to pass to server
my $file = '/path/to/file.txt'; # [File] file to upload

eval { 
    my $result = $api->upload_file(pet_id => $pet_id, additional_metadata => $additional_metadata, file => $file);
};
if ($@) {
    warn "Exception when calling upload_file: $@\n";
}
```

### Parameters
Required | Name | Type | Description 
------------ | ------------- | ------------- | -------------
 Yes | pet_id | int | ID of pet to update
 No | additional_metadata | string | Additional data to pass to server
 No | file | File | file to upload

### Return type

void (empty response body)

### HTTP headers

Content-Type: multipart/form-data
Accept: application/json, application/xml

### Authentication scheme

petstore_auth 


## **get_pet_by_id_in_object**

Fake endpoint to test inline arbitrary object return by 'Find pet by ID'

Returns a pet when ID < 10.  ID > 10 or nonintegers will simulate API error conditions

### Sample 
```perl
my $api = Something::Deep::PetApi->new();
my $pet_id = 789; # [int] ID of pet that needs to be fetched

eval { 
    my $result = $api->get_pet_by_id_in_object(pet_id => $pet_id);
};
if ($@) {
    warn "Exception when calling get_pet_by_id_in_object: $@\n";
}
```

### Parameters
Required | Name | Type | Description 
------------ | ------------- | ------------- | -------------
 Yes | pet_id | int | ID of pet that needs to be fetched

### Return type

InlineResponse200

### HTTP headers

Content-Type: Not defined
Accept: application/json, application/xml

### Authentication scheme

api_key petstore_auth 


## **pet_pet_idtesting_byte_arraytrue_get**

Fake endpoint to test byte array return by 'Find pet by ID'

Returns a pet when ID < 10.  ID > 10 or nonintegers will simulate API error conditions

### Sample 
```perl
my $api = Something::Deep::PetApi->new();
my $pet_id = 789; # [int] ID of pet that needs to be fetched

eval { 
    my $result = $api->pet_pet_idtesting_byte_arraytrue_get(pet_id => $pet_id);
};
if ($@) {
    warn "Exception when calling pet_pet_idtesting_byte_arraytrue_get: $@\n";
}
```

### Parameters
Required | Name | Type | Description 
------------ | ------------- | ------------- | -------------
 Yes | pet_id | int | ID of pet that needs to be fetched

### Return type

string

### HTTP headers

Content-Type: Not defined
Accept: application/json, application/xml

### Authentication scheme

api_key petstore_auth 


## **add_pet_using_byte_array**

Fake endpoint to test byte array in body parameter for adding a new pet to the store



### Sample 
```perl
my $api = Something::Deep::PetApi->new();
my $body = new Something::Deep::Object::string->new(); # [string] Pet object in the form of byte array

eval { 
    my $result = $api->add_pet_using_byte_array(body => $body);
};
if ($@) {
    warn "Exception when calling add_pet_using_byte_array: $@\n";
}
```

### Parameters
Required | Name | Type | Description 
------------ | ------------- | ------------- | -------------
 No | body | string | Pet object in the form of byte array

### Return type

void (empty response body)

### HTTP headers

Content-Type: application/json, application/xml
Accept: application/json, application/xml

### Authentication scheme

petstore_auth 


1;
