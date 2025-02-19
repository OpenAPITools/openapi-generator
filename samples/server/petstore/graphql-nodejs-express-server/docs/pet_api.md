# pet_api

All URIs are relative to *http://petstore.swagger.io/v2*

Method | HTTP request | Description
------------- | ------------- | -------------
[**AddPet**](pet_api.md#AddPet) | **POST** /pet | Add a new pet to the store
[**DeletePet**](pet_api.md#DeletePet) | **DELETE** /pet/{petId} | Deletes a pet
[**FindPetsByStatus**](pet_api.md#FindPetsByStatus) | **GET** /pet/findByStatus | Finds Pets by status
[**FindPetsByTags**](pet_api.md#FindPetsByTags) | **GET** /pet/findByTags | Finds Pets by tags
[**GetPetById**](pet_api.md#GetPetById) | **GET** /pet/{petId} | Find pet by ID
[**UpdatePet**](pet_api.md#UpdatePet) | **PUT** /pet | Update an existing pet
[**UpdatePetWithForm**](pet_api.md#UpdatePetWithForm) | **POST** /pet/{petId} | Updates a pet in the store with form data
[**UploadFile**](pet_api.md#UploadFile) | **POST** /pet/{petId}/uploadImage | uploads an image


<a name="AddPet"></a>
# **AddPet**
> AddPet(body)

Add a new pet to the store
<a name="DeletePet"></a>
# **DeletePet**
> DeletePet(petId, apiKey)

Deletes a pet
<a name="FindPetsByStatus"></a>
# **FindPetsByStatus**
> Pet FindPetsByStatus(status)

Finds Pets by status

Multiple status values can be provided with comma separated strings
<a name="FindPetsByTags"></a>
# **FindPetsByTags**
> Pet FindPetsByTags(tags)

Finds Pets by tags

Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
<a name="GetPetById"></a>
# **GetPetById**
> Pet GetPetById(petId)

Find pet by ID

Returns a single pet
<a name="UpdatePet"></a>
# **UpdatePet**
> UpdatePet(body)

Update an existing pet
<a name="UpdatePetWithForm"></a>
# **UpdatePetWithForm**
> UpdatePetWithForm(petId, name, status)

Updates a pet in the store with form data
<a name="UploadFile"></a>
# **UploadFile**
> ApiResponse UploadFile(petId, additionalMetadata, file)

uploads an image
