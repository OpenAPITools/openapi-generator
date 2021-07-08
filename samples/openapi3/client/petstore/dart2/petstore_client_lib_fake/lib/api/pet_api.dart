//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
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
  Future<Response> addPetWithHttpInfo(Pet pet) async {
    // Verify required params are set.
    if (pet == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: pet');
    }

    final path = r'/pet';

    Object postBody = pet;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    final contentTypes = <String>['application/json', 'application/xml'];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>['petstore_auth'];


    return await apiClient.invokeAPI(
      path,
      'POST',
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
      authNames,
    );
  }

  /// Add a new pet to the store
  ///
  /// Parameters:
  ///
  /// * [Pet] pet (required):
  ///   Pet object that needs to be added to the store
  Future<void> addPet(Pet pet) async {
    final response = await addPetWithHttpInfo(pet);
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
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
  Future<Response> deletePetWithHttpInfo(int petId, { String apiKey }) async {
    // Verify required params are set.
    if (petId == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: petId');
    }

    final path = r'/pet/{petId}'
      .replaceAll('{' + 'petId' + '}', petId.toString());

    Object postBody;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    if (apiKey != null) {
      headerParams[r'api_key'] = parameterToString(apiKey);
    }

    final contentTypes = <String>[];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>['petstore_auth'];


    return await apiClient.invokeAPI(
      path,
      'DELETE',
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
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
  Future<void> deletePet(int petId, { String apiKey }) async {
    final response = await deletePetWithHttpInfo(petId,  apiKey: apiKey );
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
  Future<Response> findPetsByStatusWithHttpInfo(List<String> status) async {
    // Verify required params are set.
    if (status == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: status');
    }

    final path = r'/pet/findByStatus';

    Object postBody;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

      queryParams.addAll(_convertParametersForCollectionFormat('csv', 'status', status));

    final contentTypes = <String>[];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>['petstore_auth'];


    return await apiClient.invokeAPI(
      path,
      'GET',
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
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
  Future<List<Pet>> findPetsByStatus(List<String> status) async {
    final response = await findPetsByStatusWithHttpInfo(status);
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
    // When a remote server returns no body with a status of 204, we shall not decode it.
    // At the time of writing this, `dart:convert` will throw an "Unexpected end of input"
    // FormatException when trying to decode an empty string.
    if (response.body != null && response.statusCode != HttpStatus.noContent) {
      return (await apiClient.deserializeAsync(await _decodeBodyBytes(response), 'List<Pet>') as List)
        .cast<Pet>()
        .toList(growable: false);
    }
    return Future<List<Pet>>.value(null);
  }

  /// Finds Pets by tags
  ///
  /// Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
  ///
  /// Note: This method returns the HTTP [Response].
  ///
  /// Parameters:
  ///
  /// * [Set<String>] tags (required):
  ///   Tags to filter by
  Future<Response> findPetsByTagsWithHttpInfo(Set<String> tags) async {
    // Verify required params are set.
    if (tags == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: tags');
    }

    final path = r'/pet/findByTags';

    Object postBody;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

      queryParams.addAll(_convertParametersForCollectionFormat('csv', 'tags', tags));

    final contentTypes = <String>[];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>['petstore_auth'];


    return await apiClient.invokeAPI(
      path,
      'GET',
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
      authNames,
    );
  }

  /// Finds Pets by tags
  ///
  /// Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
  ///
  /// Parameters:
  ///
  /// * [Set<String>] tags (required):
  ///   Tags to filter by
  Future<Set<Pet>> findPetsByTags(Set<String> tags) async {
    final response = await findPetsByTagsWithHttpInfo(tags);
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
    // When a remote server returns no body with a status of 204, we shall not decode it.
    // At the time of writing this, `dart:convert` will throw an "Unexpected end of input"
    // FormatException when trying to decode an empty string.
    if (response.body != null && response.statusCode != HttpStatus.noContent) {
      return (await apiClient.deserializeAsync(await _decodeBodyBytes(response), 'Set<Pet>') as List)
        .cast<Pet>()
        .toSet();
    }
    return Future<Set<Pet>>.value(null);
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
  Future<Response> getPetByIdWithHttpInfo(int petId) async {
    // Verify required params are set.
    if (petId == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: petId');
    }

    final path = r'/pet/{petId}'
      .replaceAll('{' + 'petId' + '}', petId.toString());

    Object postBody;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    final contentTypes = <String>[];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>['api_key'];


    return await apiClient.invokeAPI(
      path,
      'GET',
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
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
  Future<Pet> getPetById(int petId) async {
    final response = await getPetByIdWithHttpInfo(petId);
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
    // When a remote server returns no body with a status of 204, we shall not decode it.
    // At the time of writing this, `dart:convert` will throw an "Unexpected end of input"
    // FormatException when trying to decode an empty string.
    if (response.body != null && response.statusCode != HttpStatus.noContent) {
      return await apiClient.deserializeAsync(await _decodeBodyBytes(response), 'Pet',) as Pet;
        }
    return Future<Pet>.value(null);
  }

  /// Update an existing pet
  ///
  /// Note: This method returns the HTTP [Response].
  ///
  /// Parameters:
  ///
  /// * [Pet] pet (required):
  ///   Pet object that needs to be added to the store
  Future<Response> updatePetWithHttpInfo(Pet pet) async {
    // Verify required params are set.
    if (pet == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: pet');
    }

    final path = r'/pet';

    Object postBody = pet;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    final contentTypes = <String>['application/json', 'application/xml'];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>['petstore_auth'];


    return await apiClient.invokeAPI(
      path,
      'PUT',
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
      authNames,
    );
  }

  /// Update an existing pet
  ///
  /// Parameters:
  ///
  /// * [Pet] pet (required):
  ///   Pet object that needs to be added to the store
  Future<void> updatePet(Pet pet) async {
    final response = await updatePetWithHttpInfo(pet);
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
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
  Future<Response> updatePetWithFormWithHttpInfo(int petId, { String name, String status }) async {
    // Verify required params are set.
    if (petId == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: petId');
    }

    final path = r'/pet/{petId}'
      .replaceAll('{' + 'petId' + '}', petId.toString());

    Object postBody;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    final contentTypes = <String>['application/x-www-form-urlencoded'];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>['petstore_auth'];

    if (name != null) {
      formParams[r'name'] = parameterToString(name);
    }
    if (status != null) {
      formParams[r'status'] = parameterToString(status);
    }

    return await apiClient.invokeAPI(
      path,
      'POST',
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
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
  Future<void> updatePetWithForm(int petId, { String name, String status }) async {
    final response = await updatePetWithFormWithHttpInfo(petId,  name: name, status: status );
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
  Future<Response> uploadFileWithHttpInfo(int petId, { String additionalMetadata, MultipartFile file }) async {
    // Verify required params are set.
    if (petId == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: petId');
    }

    final path = r'/pet/{petId}/uploadImage'
      .replaceAll('{' + 'petId' + '}', petId.toString());

    Object postBody;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    final contentTypes = <String>['multipart/form-data'];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>['petstore_auth'];

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

    return await apiClient.invokeAPI(
      path,
      'POST',
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
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
  Future<ApiResponse> uploadFile(int petId, { String additionalMetadata, MultipartFile file }) async {
    final response = await uploadFileWithHttpInfo(petId,  additionalMetadata: additionalMetadata, file: file );
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
    // When a remote server returns no body with a status of 204, we shall not decode it.
    // At the time of writing this, `dart:convert` will throw an "Unexpected end of input"
    // FormatException when trying to decode an empty string.
    if (response.body != null && response.statusCode != HttpStatus.noContent) {
      return await apiClient.deserializeAsync(await _decodeBodyBytes(response), 'ApiResponse',) as ApiResponse;
        }
    return Future<ApiResponse>.value(null);
  }

  /// uploads an image (required)
  ///
  /// Note: This method returns the HTTP [Response].
  ///
  /// Parameters:
  ///
  /// * [int] petId (required):
  ///   ID of pet to update
  ///
  /// * [MultipartFile] requiredFile (required):
  ///   file to upload
  ///
  /// * [String] additionalMetadata:
  ///   Additional data to pass to server
  Future<Response> uploadFileWithRequiredFileWithHttpInfo(int petId, MultipartFile requiredFile, { String additionalMetadata }) async {
    // Verify required params are set.
    if (petId == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: petId');
    }
    if (requiredFile == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: requiredFile');
    }

    final path = r'/fake/{petId}/uploadImageWithRequiredFile'
      .replaceAll('{' + 'petId' + '}', petId.toString());

    Object postBody;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    final contentTypes = <String>['multipart/form-data'];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>['petstore_auth'];

    bool hasFields = false;
    final mp = MultipartRequest('POST', Uri.parse(path));
    if (additionalMetadata != null) {
      hasFields = true;
      mp.fields[r'additionalMetadata'] = parameterToString(additionalMetadata);
    }
    if (requiredFile != null) {
      hasFields = true;
      mp.fields[r'requiredFile'] = requiredFile.field;
      mp.files.add(requiredFile);
    }
    if (hasFields) {
      postBody = mp;
    }

    return await apiClient.invokeAPI(
      path,
      'POST',
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
      authNames,
    );
  }

  /// uploads an image (required)
  ///
  /// Parameters:
  ///
  /// * [int] petId (required):
  ///   ID of pet to update
  ///
  /// * [MultipartFile] requiredFile (required):
  ///   file to upload
  ///
  /// * [String] additionalMetadata:
  ///   Additional data to pass to server
  Future<ApiResponse> uploadFileWithRequiredFile(int petId, MultipartFile requiredFile, { String additionalMetadata }) async {
    final response = await uploadFileWithRequiredFileWithHttpInfo(petId, requiredFile,  additionalMetadata: additionalMetadata );
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
    // When a remote server returns no body with a status of 204, we shall not decode it.
    // At the time of writing this, `dart:convert` will throw an "Unexpected end of input"
    // FormatException when trying to decode an empty string.
    if (response.body != null && response.statusCode != HttpStatus.noContent) {
      return await apiClient.deserializeAsync(await _decodeBodyBytes(response), 'ApiResponse',) as ApiResponse;
        }
    return Future<ApiResponse>.value(null);
  }
}
