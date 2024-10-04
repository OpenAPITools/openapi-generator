//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.18

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;


class PetApi {
  PetApi([ApiClient? apiClient]) : apiClient = apiClient ?? defaultApiClient;

  final ApiClient apiClient;

  /// Add a new pet to the store
  ///
  /// 
  ///
  /// Note: This method returns the HTTP [Response].
  ///
  /// Parameters:
  ///
  /// * [Pet] pet (required):
  ///   Pet object that needs to be added to the store
  Future<Response> addPetWithHttpInfo(Pet pet,) async {
    // ignore: prefer_const_declarations
    final _path = r'/pet';

    // ignore: prefer_final_locals
    Object? _postBody = pet;

    final __queryParams = <QueryParam>[];
    final _headerParams = <String, String>{};
    final _formParams = <String, String>{};

    const _contentTypes = <String>['application/json', 'application/xml'];


    return apiClient.invokeAPI(
      _path,
      'POST',
      __queryParams,
      _postBody,
      _headerParams,
      _formParams,
      _contentTypes.isEmpty ? null : _contentTypes.first,
    );
  }

  /// Add a new pet to the store
  ///
  /// 
  ///
  /// Parameters:
  ///
  /// * [Pet] pet (required):
  ///   Pet object that needs to be added to the store
  Future<Pet?> addPet(Pet pet,) async {
    final _response = await addPetWithHttpInfo(pet,);
    if (_response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(_response.statusCode, await _decodeBodyBytes(_response));
    }
    // When a remote server returns no body with a status of 204, we shall not decode it.
    // At the time of writing this, `dart:convert` will throw an "Unexpected end of input"
    // FormatException when trying to decode an empty string.
    if (_response.bodyBytes.isNotEmpty && _response.statusCode != HttpStatus.noContent) {
      return await apiClient.deserializeAsync(await _decodeBodyBytes(_response), 'Pet',) as Pet;
    
    }
    return null;
  }

  /// Deletes a pet
  ///
  /// 
  ///
  /// Note: This method returns the HTTP [Response].
  ///
  /// Parameters:
  ///
  /// * [int] petId (required):
  ///   Pet id to delete
  ///
  /// * [String] apiKey:
  Future<Response> deletePetWithHttpInfo(int petId, { String? apiKey, }) async {
    // ignore: prefer_const_declarations
    final _path = r'/pet/{petId}'
      .replaceAll('{petId}', petId.toString());

    // ignore: prefer_final_locals
    Object? _postBody;

    final __queryParams = <QueryParam>[];
    final _headerParams = <String, String>{};
    final _formParams = <String, String>{};

    if (apiKey != null) {
      _headerParams[r'api_key'] = parameterToString(apiKey);
    }

    const _contentTypes = <String>[];


    return apiClient.invokeAPI(
      _path,
      'DELETE',
      __queryParams,
      _postBody,
      _headerParams,
      _formParams,
      _contentTypes.isEmpty ? null : _contentTypes.first,
    );
  }

  /// Deletes a pet
  ///
  /// 
  ///
  /// Parameters:
  ///
  /// * [int] petId (required):
  ///   Pet id to delete
  ///
  /// * [String] apiKey:
  Future<void> deletePet(int petId, { String? apiKey, }) async {
    final _response = await deletePetWithHttpInfo(petId,  apiKey: apiKey, );
    if (_response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(_response.statusCode, await _decodeBodyBytes(_response));
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
    // ignore: prefer_const_declarations
    final _path = r'/pet/findByStatus';

    // ignore: prefer_final_locals
    Object? _postBody;

    final __queryParams = <QueryParam>[];
    final _headerParams = <String, String>{};
    final _formParams = <String, String>{};

      __queryParams.addAll(_queryParams('csv', 'status', status));

    const _contentTypes = <String>[];


    return apiClient.invokeAPI(
      _path,
      'GET',
      __queryParams,
      _postBody,
      _headerParams,
      _formParams,
      _contentTypes.isEmpty ? null : _contentTypes.first,
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
  Future<List<Pet>?> findPetsByStatus(List<String> status,) async {
    final _response = await findPetsByStatusWithHttpInfo(status,);
    if (_response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(_response.statusCode, await _decodeBodyBytes(_response));
    }
    // When a remote server returns no body with a status of 204, we shall not decode it.
    // At the time of writing this, `dart:convert` will throw an "Unexpected end of input"
    // FormatException when trying to decode an empty string.
    if (_response.bodyBytes.isNotEmpty && _response.statusCode != HttpStatus.noContent) {
      final _responseBody = await _decodeBodyBytes(_response);
      return (await apiClient.deserializeAsync(_responseBody, 'List<Pet>') as List)
        .cast<Pet>()
        .toList(growable: false);

    }
    return null;
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
    // ignore: prefer_const_declarations
    final _path = r'/pet/findByTags';

    // ignore: prefer_final_locals
    Object? _postBody;

    final __queryParams = <QueryParam>[];
    final _headerParams = <String, String>{};
    final _formParams = <String, String>{};

      __queryParams.addAll(_queryParams('csv', 'tags', tags));

    const _contentTypes = <String>[];


    return apiClient.invokeAPI(
      _path,
      'GET',
      __queryParams,
      _postBody,
      _headerParams,
      _formParams,
      _contentTypes.isEmpty ? null : _contentTypes.first,
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
  Future<List<Pet>?> findPetsByTags(List<String> tags,) async {
    final _response = await findPetsByTagsWithHttpInfo(tags,);
    if (_response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(_response.statusCode, await _decodeBodyBytes(_response));
    }
    // When a remote server returns no body with a status of 204, we shall not decode it.
    // At the time of writing this, `dart:convert` will throw an "Unexpected end of input"
    // FormatException when trying to decode an empty string.
    if (_response.bodyBytes.isNotEmpty && _response.statusCode != HttpStatus.noContent) {
      final _responseBody = await _decodeBodyBytes(_response);
      return (await apiClient.deserializeAsync(_responseBody, 'List<Pet>') as List)
        .cast<Pet>()
        .toList(growable: false);

    }
    return null;
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
    // ignore: prefer_const_declarations
    final _path = r'/pet/{petId}'
      .replaceAll('{petId}', petId.toString());

    // ignore: prefer_final_locals
    Object? _postBody;

    final __queryParams = <QueryParam>[];
    final _headerParams = <String, String>{};
    final _formParams = <String, String>{};

    const _contentTypes = <String>[];


    return apiClient.invokeAPI(
      _path,
      'GET',
      __queryParams,
      _postBody,
      _headerParams,
      _formParams,
      _contentTypes.isEmpty ? null : _contentTypes.first,
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
  Future<Pet?> getPetById(int petId,) async {
    final _response = await getPetByIdWithHttpInfo(petId,);
    if (_response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(_response.statusCode, await _decodeBodyBytes(_response));
    }
    // When a remote server returns no body with a status of 204, we shall not decode it.
    // At the time of writing this, `dart:convert` will throw an "Unexpected end of input"
    // FormatException when trying to decode an empty string.
    if (_response.bodyBytes.isNotEmpty && _response.statusCode != HttpStatus.noContent) {
      return await apiClient.deserializeAsync(await _decodeBodyBytes(_response), 'Pet',) as Pet;
    
    }
    return null;
  }

  /// Update an existing pet
  ///
  /// 
  ///
  /// Note: This method returns the HTTP [Response].
  ///
  /// Parameters:
  ///
  /// * [Pet] pet (required):
  ///   Pet object that needs to be added to the store
  Future<Response> updatePetWithHttpInfo(Pet pet,) async {
    // ignore: prefer_const_declarations
    final _path = r'/pet';

    // ignore: prefer_final_locals
    Object? _postBody = pet;

    final __queryParams = <QueryParam>[];
    final _headerParams = <String, String>{};
    final _formParams = <String, String>{};

    const _contentTypes = <String>['application/json', 'application/xml'];


    return apiClient.invokeAPI(
      _path,
      'PUT',
      __queryParams,
      _postBody,
      _headerParams,
      _formParams,
      _contentTypes.isEmpty ? null : _contentTypes.first,
    );
  }

  /// Update an existing pet
  ///
  /// 
  ///
  /// Parameters:
  ///
  /// * [Pet] pet (required):
  ///   Pet object that needs to be added to the store
  Future<Pet?> updatePet(Pet pet,) async {
    final _response = await updatePetWithHttpInfo(pet,);
    if (_response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(_response.statusCode, await _decodeBodyBytes(_response));
    }
    // When a remote server returns no body with a status of 204, we shall not decode it.
    // At the time of writing this, `dart:convert` will throw an "Unexpected end of input"
    // FormatException when trying to decode an empty string.
    if (_response.bodyBytes.isNotEmpty && _response.statusCode != HttpStatus.noContent) {
      return await apiClient.deserializeAsync(await _decodeBodyBytes(_response), 'Pet',) as Pet;
    
    }
    return null;
  }

  /// Updates a pet in the store with form data
  ///
  /// 
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
  Future<Response> updatePetWithFormWithHttpInfo(int petId, { String? name, String? status, }) async {
    // ignore: prefer_const_declarations
    final _path = r'/pet/{petId}'
      .replaceAll('{petId}', petId.toString());

    // ignore: prefer_final_locals
    Object? _postBody;

    final __queryParams = <QueryParam>[];
    final _headerParams = <String, String>{};
    final _formParams = <String, String>{};

    const _contentTypes = <String>['application/x-www-form-urlencoded'];

    if (name != null) {
      _formParams[r'name'] = parameterToString(name);
    }
    if (status != null) {
      _formParams[r'status'] = parameterToString(status);
    }

    return apiClient.invokeAPI(
      _path,
      'POST',
      __queryParams,
      _postBody,
      _headerParams,
      _formParams,
      _contentTypes.isEmpty ? null : _contentTypes.first,
    );
  }

  /// Updates a pet in the store with form data
  ///
  /// 
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
  Future<void> updatePetWithForm(int petId, { String? name, String? status, }) async {
    final _response = await updatePetWithFormWithHttpInfo(petId,  name: name, status: status, );
    if (_response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(_response.statusCode, await _decodeBodyBytes(_response));
    }
  }

  /// uploads an image
  ///
  /// 
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
  Future<Response> uploadFileWithHttpInfo(int petId, { String? additionalMetadata, MultipartFile? file, }) async {
    // ignore: prefer_const_declarations
    final _path = r'/pet/{petId}/uploadImage'
      .replaceAll('{petId}', petId.toString());

    // ignore: prefer_final_locals
    Object? _postBody;

    final __queryParams = <QueryParam>[];
    final _headerParams = <String, String>{};
    final _formParams = <String, String>{};

    const _contentTypes = <String>['multipart/form-data'];

    bool _hasFields = false;
    final _mp = MultipartRequest('POST', Uri.parse(_path));
    if (additionalMetadata != null) {
      _hasFields = true;
      _mp.fields[r'additionalMetadata'] = parameterToString(additionalMetadata);
    }
    if (file != null) {
      _hasFields = true;
      _mp.fields[r'file'] = file.field;
      _mp.files.add(file);
    }
    if (_hasFields) {
      _postBody = _mp;
    }

    return apiClient.invokeAPI(
      _path,
      'POST',
      __queryParams,
      _postBody,
      _headerParams,
      _formParams,
      _contentTypes.isEmpty ? null : _contentTypes.first,
    );
  }

  /// uploads an image
  ///
  /// 
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
  Future<ApiResponse?> uploadFile(int petId, { String? additionalMetadata, MultipartFile? file, }) async {
    final _response = await uploadFileWithHttpInfo(petId,  additionalMetadata: additionalMetadata, file: file, );
    if (_response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(_response.statusCode, await _decodeBodyBytes(_response));
    }
    // When a remote server returns no body with a status of 204, we shall not decode it.
    // At the time of writing this, `dart:convert` will throw an "Unexpected end of input"
    // FormatException when trying to decode an empty string.
    if (_response.bodyBytes.isNotEmpty && _response.statusCode != HttpStatus.noContent) {
      return await apiClient.deserializeAsync(await _decodeBodyBytes(_response), 'ApiResponse',) as ApiResponse;
    
    }
    return null;
  }
}
