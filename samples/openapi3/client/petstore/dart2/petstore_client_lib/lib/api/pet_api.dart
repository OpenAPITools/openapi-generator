//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;


class PetApi {
  PetApi([ApiClient apiClient]) : apiClient = apiClient ?? defaultApiClient;

  final ApiClient apiClient;

  /// Add a new pet to the store
  ///
  /// Note: This method returns the HTTP [Response].
  ///
  /// Parameters:
  ///
  /// * [Pet] pet (required):
  ///   Pet object that needs to be added to the store
  Future<Response> addPetWithHttpInfo(Pet pet,) async {
    // Verify required params are set.
    if (pet == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: pet');
    }

    // ignore: prefer_const_declarations
    final path = r'/pet';

    // ignore: prefer_final_locals
    Object postBody = pet;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    const authNames = <String>['petstore_auth'];
    const contentTypes = <String>['application/json', 'application/xml'];


    return apiClient.invokeAPI(
      path,
      'POST',
      queryParams,
      postBody,
      headerParams,
      formParams,
      contentTypes.isEmpty ? null : contentTypes[0],
      authNames,
    );
  }

  /// Add a new pet to the store
  ///
  /// Parameters:
  ///
  /// * [Pet] pet (required):
  ///   Pet object that needs to be added to the store
  Future<Pet> addPet(Pet pet,) async {
    final response = await addPetWithHttpInfo(pet,);
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
    // When a remote server returns no body with a status of 204, we shall not decode it.
    // At the time of writing this, `dart:convert` will throw an "Unexpected end of input"
    // FormatException when trying to decode an empty string.
    if (response.body != null && response.statusCode != HttpStatus.noContent) {
      return await apiClient.deserializeAsync(await _decodeBodyBytes(response), 'Pet',) as Pet;
    
    }
    return Future<Pet>.value();
  }

  /// Deletes a pet
  ///
  /// Note: This method returns the HTTP [Response].
  ///
  /// Parameters:
  ///
  /// * [int] petId (required):
  ///   Pet id to delete
  ///
  /// * [String] apiKey:
  Future<Response> deletePetWithHttpInfo(int petId, { String apiKey, }) async {
    // Verify required params are set.
    if (petId == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: petId');
    }

    // ignore: prefer_const_declarations
    final path = r'/pet/{petId}'
      .replaceAll('{petId}', petId.toString());

    // ignore: prefer_final_locals
    Object postBody;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    if (apiKey != null) {
      headerParams[r'api_key'] = parameterToString(apiKey);
    }

    const authNames = <String>['petstore_auth'];
    const contentTypes = <String>[];


    return apiClient.invokeAPI(
      path,
      'DELETE',
      queryParams,
      postBody,
      headerParams,
      formParams,
      contentTypes.isEmpty ? null : contentTypes[0],
      authNames,
    );
  }

  /// Deletes a pet
  ///
  /// Parameters:
  ///
  /// * [int] petId (required):
  ///   Pet id to delete
  ///
  /// * [String] apiKey:
  Future<void> deletePet(int petId, { String apiKey, }) async {
    final response = await deletePetWithHttpInfo(petId,  apiKey: apiKey, );
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
  }

  /// Finds Pets by status
  ///
  /// Multiple status values can be provided with comma separated strings
  ///
  /// Note: This method returns the HTTP [Response].
  ///
  /// Parameters:
  ///
  /// * [List<String>] status (required):
  ///   Status values that need to be considered for filter
  Future<Response> findPetsByStatusWithHttpInfo(List<String> status,) async {
    // Verify required params are set.
    if (status == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: status');
    }

    // ignore: prefer_const_declarations
    final path = r'/pet/findByStatus';

    // ignore: prefer_final_locals
    Object postBody;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

      queryParams.addAll(_convertParametersForCollectionFormat('csv', 'status', status));

    const authNames = <String>['petstore_auth'];
    const contentTypes = <String>[];


    return apiClient.invokeAPI(
      path,
      'GET',
      queryParams,
      postBody,
      headerParams,
      formParams,
      contentTypes.isEmpty ? null : contentTypes[0],
      authNames,
    );
  }

  /// Finds Pets by status
  ///
  /// Multiple status values can be provided with comma separated strings
  ///
  /// Parameters:
  ///
  /// * [List<String>] status (required):
  ///   Status values that need to be considered for filter
  Future<List<Pet>> findPetsByStatus(List<String> status,) async {
    final response = await findPetsByStatusWithHttpInfo(status,);
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
    // When a remote server returns no body with a status of 204, we shall not decode it.
    // At the time of writing this, `dart:convert` will throw an "Unexpected end of input"
    // FormatException when trying to decode an empty string.
    if (response.body != null && response.statusCode != HttpStatus.noContent) {
      final responseBody = await _decodeBodyBytes(response);
      return (await apiClient.deserializeAsync(responseBody, 'List<Pet>') as List)
        .cast<Pet>()
        .toList(growable: false);

    }
    return Future<List<Pet>>.value();
  }

  /// Finds Pets by tags
  ///
  /// Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
  ///
  /// Note: This method returns the HTTP [Response].
  ///
  /// Parameters:
  ///
  /// * [List<String>] tags (required):
  ///   Tags to filter by
  Future<Response> findPetsByTagsWithHttpInfo(List<String> tags,) async {
    // Verify required params are set.
    if (tags == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: tags');
    }

    // ignore: prefer_const_declarations
    final path = r'/pet/findByTags';

    // ignore: prefer_final_locals
    Object postBody;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

      queryParams.addAll(_convertParametersForCollectionFormat('csv', 'tags', tags));

    const authNames = <String>['petstore_auth'];
    const contentTypes = <String>[];


    return apiClient.invokeAPI(
      path,
      'GET',
      queryParams,
      postBody,
      headerParams,
      formParams,
      contentTypes.isEmpty ? null : contentTypes[0],
      authNames,
    );
  }

  /// Finds Pets by tags
  ///
  /// Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
  ///
  /// Parameters:
  ///
  /// * [List<String>] tags (required):
  ///   Tags to filter by
  Future<List<Pet>> findPetsByTags(List<String> tags,) async {
    final response = await findPetsByTagsWithHttpInfo(tags,);
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
    // When a remote server returns no body with a status of 204, we shall not decode it.
    // At the time of writing this, `dart:convert` will throw an "Unexpected end of input"
    // FormatException when trying to decode an empty string.
    if (response.body != null && response.statusCode != HttpStatus.noContent) {
      final responseBody = await _decodeBodyBytes(response);
      return (await apiClient.deserializeAsync(responseBody, 'List<Pet>') as List)
        .cast<Pet>()
        .toList(growable: false);

    }
    return Future<List<Pet>>.value();
  }

  /// Find pet by ID
  ///
  /// Returns a single pet
  ///
  /// Note: This method returns the HTTP [Response].
  ///
  /// Parameters:
  ///
  /// * [int] petId (required):
  ///   ID of pet to return
  Future<Response> getPetByIdWithHttpInfo(int petId,) async {
    // Verify required params are set.
    if (petId == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: petId');
    }

    // ignore: prefer_const_declarations
    final path = r'/pet/{petId}'
      .replaceAll('{petId}', petId.toString());

    // ignore: prefer_final_locals
    Object postBody;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    const authNames = <String>['api_key'];
    const contentTypes = <String>[];


    return apiClient.invokeAPI(
      path,
      'GET',
      queryParams,
      postBody,
      headerParams,
      formParams,
      contentTypes.isEmpty ? null : contentTypes[0],
      authNames,
    );
  }

  /// Find pet by ID
  ///
  /// Returns a single pet
  ///
  /// Parameters:
  ///
  /// * [int] petId (required):
  ///   ID of pet to return
  Future<Pet> getPetById(int petId,) async {
    final response = await getPetByIdWithHttpInfo(petId,);
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
    // When a remote server returns no body with a status of 204, we shall not decode it.
    // At the time of writing this, `dart:convert` will throw an "Unexpected end of input"
    // FormatException when trying to decode an empty string.
    if (response.body != null && response.statusCode != HttpStatus.noContent) {
      return await apiClient.deserializeAsync(await _decodeBodyBytes(response), 'Pet',) as Pet;
    
    }
    return Future<Pet>.value();
  }

  /// Update an existing pet
  ///
  /// Note: This method returns the HTTP [Response].
  ///
  /// Parameters:
  ///
  /// * [Pet] pet (required):
  ///   Pet object that needs to be added to the store
  Future<Response> updatePetWithHttpInfo(Pet pet,) async {
    // Verify required params are set.
    if (pet == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: pet');
    }

    // ignore: prefer_const_declarations
    final path = r'/pet';

    // ignore: prefer_final_locals
    Object postBody = pet;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    const authNames = <String>['petstore_auth'];
    const contentTypes = <String>['application/json', 'application/xml'];


    return apiClient.invokeAPI(
      path,
      'PUT',
      queryParams,
      postBody,
      headerParams,
      formParams,
      contentTypes.isEmpty ? null : contentTypes[0],
      authNames,
    );
  }

  /// Update an existing pet
  ///
  /// Parameters:
  ///
  /// * [Pet] pet (required):
  ///   Pet object that needs to be added to the store
  Future<Pet> updatePet(Pet pet,) async {
    final response = await updatePetWithHttpInfo(pet,);
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
    // When a remote server returns no body with a status of 204, we shall not decode it.
    // At the time of writing this, `dart:convert` will throw an "Unexpected end of input"
    // FormatException when trying to decode an empty string.
    if (response.body != null && response.statusCode != HttpStatus.noContent) {
      return await apiClient.deserializeAsync(await _decodeBodyBytes(response), 'Pet',) as Pet;
    
    }
    return Future<Pet>.value();
  }

  /// Updates a pet in the store with form data
  ///
  /// Note: This method returns the HTTP [Response].
  ///
  /// Parameters:
  ///
  /// * [int] petId (required):
  ///   ID of pet that needs to be updated
  ///
  /// * [String] name:
  ///   Updated name of the pet
  ///
  /// * [String] status:
  ///   Updated status of the pet
  Future<Response> updatePetWithFormWithHttpInfo(int petId, { String name, String status, }) async {
    // Verify required params are set.
    if (petId == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: petId');
    }

    // ignore: prefer_const_declarations
    final path = r'/pet/{petId}'
      .replaceAll('{petId}', petId.toString());

    // ignore: prefer_final_locals
    Object postBody;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    const authNames = <String>['petstore_auth'];
    const contentTypes = <String>['application/x-www-form-urlencoded'];

    if (name != null) {
      formParams[r'name'] = parameterToString(name);
    }
    if (status != null) {
      formParams[r'status'] = parameterToString(status);
    }

    return apiClient.invokeAPI(
      path,
      'POST',
      queryParams,
      postBody,
      headerParams,
      formParams,
      contentTypes.isEmpty ? null : contentTypes[0],
      authNames,
    );
  }

  /// Updates a pet in the store with form data
  ///
  /// Parameters:
  ///
  /// * [int] petId (required):
  ///   ID of pet that needs to be updated
  ///
  /// * [String] name:
  ///   Updated name of the pet
  ///
  /// * [String] status:
  ///   Updated status of the pet
  Future<void> updatePetWithForm(int petId, { String name, String status, }) async {
    final response = await updatePetWithFormWithHttpInfo(petId,  name: name, status: status, );
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
  }

  /// uploads an image
  ///
  /// Note: This method returns the HTTP [Response].
  ///
  /// Parameters:
  ///
  /// * [int] petId (required):
  ///   ID of pet to update
  ///
  /// * [String] additionalMetadata:
  ///   Additional data to pass to server
  ///
  /// * [MultipartFile] file:
  ///   file to upload
  Future<Response> uploadFileWithHttpInfo(int petId, { String additionalMetadata, MultipartFile file, }) async {
    // Verify required params are set.
    if (petId == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: petId');
    }

    // ignore: prefer_const_declarations
    final path = r'/pet/{petId}/uploadImage'
      .replaceAll('{petId}', petId.toString());

    // ignore: prefer_final_locals
    Object postBody;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    const authNames = <String>['petstore_auth'];
    const contentTypes = <String>['multipart/form-data'];

    bool hasFields = false;
    final mp = MultipartRequest('POST', Uri.parse(path));
    if (additionalMetadata != null) {
      hasFields = true;
      mp.fields[r'additionalMetadata'] = parameterToString(additionalMetadata);
    }
    if (file != null) {
      hasFields = true;
      mp.fields[r'file'] = file.field;
      mp.files.add(file);
    }
    if (hasFields) {
      postBody = mp;
    }

    return apiClient.invokeAPI(
      path,
      'POST',
      queryParams,
      postBody,
      headerParams,
      formParams,
      contentTypes.isEmpty ? null : contentTypes[0],
      authNames,
    );
  }

  /// uploads an image
  ///
  /// Parameters:
  ///
  /// * [int] petId (required):
  ///   ID of pet to update
  ///
  /// * [String] additionalMetadata:
  ///   Additional data to pass to server
  ///
  /// * [MultipartFile] file:
  ///   file to upload
  Future<ApiResponse> uploadFile(int petId, { String additionalMetadata, MultipartFile file, }) async {
    final response = await uploadFileWithHttpInfo(petId,  additionalMetadata: additionalMetadata, file: file, );
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
    // When a remote server returns no body with a status of 204, we shall not decode it.
    // At the time of writing this, `dart:convert` will throw an "Unexpected end of input"
    // FormatException when trying to decode an empty string.
    if (response.body != null && response.statusCode != HttpStatus.noContent) {
      return await apiClient.deserializeAsync(await _decodeBodyBytes(response), 'ApiResponse',) as ApiResponse;
    
    }
    return Future<ApiResponse>.value();
  }
}
