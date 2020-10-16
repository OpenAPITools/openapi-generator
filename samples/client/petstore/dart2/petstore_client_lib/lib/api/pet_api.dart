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

  /// Executes the same logic as [addPet] but instead returns a
  /// [StreamedResponse] that clients can listen to, for example to monitor the progress of
  /// the network activity.
  Future<StreamedResponse> addPetAsStreamedResponse(Pet body) async {
    // Verify required params are set.
    if (body == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: body');
    }

    final path = '/pet'.replaceAll('{format}', 'json');

    Object postBody = body;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};


    final contentTypes = <String>['application/json', 'application/xml'];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>['petstore_auth'];

    if (
      nullableContentType != null &&
      nullableContentType.toLowerCase().startsWith('multipart/form-data')
    ) {
      bool hasFields = false;
      final mp = MultipartRequest(null, null);
      if (hasFields) {
        postBody = mp;
      }
    }

    return await apiClient.streamAPI(
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

  /// Executes the same logic as [addPet] but instead returns the processed result as
  /// an HTTP [Response].
  Future<Response> addPetWithHttpInfo(Pet body) =>
    apiClient.getResponse(addPetAsStreamedResponse(body));

  /// Add a new pet to the store
  ///
  /// Note: This method returns nothing.
  ///
  /// Parameters:
  ///
  /// * [Pet] body (required):
  ///   Pet object that needs to be added to the store
  Future<void> addPet(Pet body) async {
    final response = await addPetWithHttpInfo(body);
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, _decodeBodyBytes(response));
    }
  }

  /// Executes the same logic as [deletePet] but instead returns a
  /// [StreamedResponse] that clients can listen to, for example to monitor the progress of
  /// the network activity.
  Future<StreamedResponse> deletePetAsStreamedResponse(int petId, { String apiKey }) async {
    // Verify required params are set.
    if (petId == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: petId');
    }

    final path = '/pet/{petId}'.replaceAll('{format}', 'json')
      .replaceAll('{' + 'petId' + '}', petId.toString());

    Object postBody;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    headerParams['api_key'] = apiKey;

    final contentTypes = <String>[];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>['petstore_auth'];

    if (
      nullableContentType != null &&
      nullableContentType.toLowerCase().startsWith('multipart/form-data')
    ) {
      bool hasFields = false;
      final mp = MultipartRequest(null, null);
      if (hasFields) {
        postBody = mp;
      }
    }

    return await apiClient.streamAPI(
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

  /// Executes the same logic as [deletePet] but instead returns the processed result as
  /// an HTTP [Response].
  Future<Response> deletePetWithHttpInfo(int petId, { String apiKey }) =>
    apiClient.getResponse(deletePetAsStreamedResponse(petId,  apiKey: apiKey ));

  /// Deletes a pet
  ///
  /// Note: This method returns nothing.
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
      throw ApiException(response.statusCode, _decodeBodyBytes(response));
    }
  }

  /// Executes the same logic as [findPetsByStatus] but instead returns a
  /// [StreamedResponse] that clients can listen to, for example to monitor the progress of
  /// the network activity.
  Future<StreamedResponse> findPetsByStatusAsStreamedResponse(List<String> status) async {
    // Verify required params are set.
    if (status == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: status');
    }

    final path = '/pet/findByStatus'.replaceAll('{format}', 'json');

    Object postBody;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

      queryParams.addAll(_convertParametersForCollectionFormat('csv', 'status', status));

    final contentTypes = <String>[];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>['petstore_auth'];

    if (
      nullableContentType != null &&
      nullableContentType.toLowerCase().startsWith('multipart/form-data')
    ) {
      bool hasFields = false;
      final mp = MultipartRequest(null, null);
      if (hasFields) {
        postBody = mp;
      }
    }

    return await apiClient.streamAPI(
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

  /// Executes the same logic as [findPetsByStatus] but instead returns the processed result as
  /// an HTTP [Response].
  Future<Response> findPetsByStatusWithHttpInfo(List<String> status) =>
    apiClient.getResponse(findPetsByStatusAsStreamedResponse(status));

  /// Finds Pets by status
  ///
  /// Multiple status values can be provided with comma separated strings
  ///
  /// After the method completes, the result is returned as an instance of [List<Pet>].
  ///
  /// Parameters:
  ///
  /// * [List<String>] status (required):
  ///   Status values that need to be considered for filter
  Future<List<Pet>> findPetsByStatus(List<String> status) async {
    final response = await findPetsByStatusWithHttpInfo(status);
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, _decodeBodyBytes(response));
    }
    // When a remote server returns no body with a status of 204, we shall not decode it.
    // At the time of writing this, `dart:convert` will throw an "Unexpected end of input"
    // FormatException when trying to decode an empty string.
    if (response.body != null && response.statusCode != HttpStatus.noContent) {
      return (apiClient.deserialize(_decodeBodyBytes(response), 'List<Pet>') as List)
        .map((item) => item as Pet)
        .toList(growable: false);
    }
    return null;
  }

  /// Executes the same logic as [findPetsByTags] but instead returns a
  /// [StreamedResponse] that clients can listen to, for example to monitor the progress of
  /// the network activity.
  Future<StreamedResponse> findPetsByTagsAsStreamedResponse(List<String> tags) async {
    // Verify required params are set.
    if (tags == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: tags');
    }

    final path = '/pet/findByTags'.replaceAll('{format}', 'json');

    Object postBody;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

      queryParams.addAll(_convertParametersForCollectionFormat('csv', 'tags', tags));

    final contentTypes = <String>[];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>['petstore_auth'];

    if (
      nullableContentType != null &&
      nullableContentType.toLowerCase().startsWith('multipart/form-data')
    ) {
      bool hasFields = false;
      final mp = MultipartRequest(null, null);
      if (hasFields) {
        postBody = mp;
      }
    }

    return await apiClient.streamAPI(
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

  /// Executes the same logic as [findPetsByTags] but instead returns the processed result as
  /// an HTTP [Response].
  Future<Response> findPetsByTagsWithHttpInfo(List<String> tags) =>
    apiClient.getResponse(findPetsByTagsAsStreamedResponse(tags));

  /// Finds Pets by tags
  ///
  /// Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
  ///
  /// After the method completes, the result is returned as an instance of [List<Pet>].
  ///
  /// Parameters:
  ///
  /// * [List<String>] tags (required):
  ///   Tags to filter by
  Future<List<Pet>> findPetsByTags(List<String> tags) async {
    final response = await findPetsByTagsWithHttpInfo(tags);
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, _decodeBodyBytes(response));
    }
    // When a remote server returns no body with a status of 204, we shall not decode it.
    // At the time of writing this, `dart:convert` will throw an "Unexpected end of input"
    // FormatException when trying to decode an empty string.
    if (response.body != null && response.statusCode != HttpStatus.noContent) {
      return (apiClient.deserialize(_decodeBodyBytes(response), 'List<Pet>') as List)
        .map((item) => item as Pet)
        .toList(growable: false);
    }
    return null;
  }

  /// Executes the same logic as [getPetById] but instead returns a
  /// [StreamedResponse] that clients can listen to, for example to monitor the progress of
  /// the network activity.
  Future<StreamedResponse> getPetByIdAsStreamedResponse(int petId) async {
    // Verify required params are set.
    if (petId == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: petId');
    }

    final path = '/pet/{petId}'.replaceAll('{format}', 'json')
      .replaceAll('{' + 'petId' + '}', petId.toString());

    Object postBody;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};


    final contentTypes = <String>[];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>['api_key'];

    if (
      nullableContentType != null &&
      nullableContentType.toLowerCase().startsWith('multipart/form-data')
    ) {
      bool hasFields = false;
      final mp = MultipartRequest(null, null);
      if (hasFields) {
        postBody = mp;
      }
    }

    return await apiClient.streamAPI(
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

  /// Executes the same logic as [getPetById] but instead returns the processed result as
  /// an HTTP [Response].
  Future<Response> getPetByIdWithHttpInfo(int petId) =>
    apiClient.getResponse(getPetByIdAsStreamedResponse(petId));

  /// Find pet by ID
  ///
  /// Returns a single pet
  ///
  /// After the method completes, the result is returned as an instance of [Pet].
  ///
  /// Parameters:
  ///
  /// * [int] petId (required):
  ///   ID of pet to return
  Future<Pet> getPetById(int petId) async {
    final response = await getPetByIdWithHttpInfo(petId);
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, _decodeBodyBytes(response));
    }
    // When a remote server returns no body with a status of 204, we shall not decode it.
    // At the time of writing this, `dart:convert` will throw an "Unexpected end of input"
    // FormatException when trying to decode an empty string.
    if (response.body != null && response.statusCode != HttpStatus.noContent) {
      return apiClient.deserialize(_decodeBodyBytes(response), 'Pet') as Pet;
    }
    return null;
  }

  /// Executes the same logic as [updatePet] but instead returns a
  /// [StreamedResponse] that clients can listen to, for example to monitor the progress of
  /// the network activity.
  Future<StreamedResponse> updatePetAsStreamedResponse(Pet body) async {
    // Verify required params are set.
    if (body == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: body');
    }

    final path = '/pet'.replaceAll('{format}', 'json');

    Object postBody = body;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};


    final contentTypes = <String>['application/json', 'application/xml'];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>['petstore_auth'];

    if (
      nullableContentType != null &&
      nullableContentType.toLowerCase().startsWith('multipart/form-data')
    ) {
      bool hasFields = false;
      final mp = MultipartRequest(null, null);
      if (hasFields) {
        postBody = mp;
      }
    }

    return await apiClient.streamAPI(
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

  /// Executes the same logic as [updatePet] but instead returns the processed result as
  /// an HTTP [Response].
  Future<Response> updatePetWithHttpInfo(Pet body) =>
    apiClient.getResponse(updatePetAsStreamedResponse(body));

  /// Update an existing pet
  ///
  /// Note: This method returns nothing.
  ///
  /// Parameters:
  ///
  /// * [Pet] body (required):
  ///   Pet object that needs to be added to the store
  Future<void> updatePet(Pet body) async {
    final response = await updatePetWithHttpInfo(body);
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, _decodeBodyBytes(response));
    }
  }

  /// Executes the same logic as [updatePetWithForm] but instead returns a
  /// [StreamedResponse] that clients can listen to, for example to monitor the progress of
  /// the network activity.
  Future<StreamedResponse> updatePetWithFormAsStreamedResponse(int petId, { String name, String status }) async {
    // Verify required params are set.
    if (petId == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: petId');
    }

    final path = '/pet/{petId}'.replaceAll('{format}', 'json')
      .replaceAll('{' + 'petId' + '}', petId.toString());

    Object postBody;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};


    final contentTypes = <String>['application/x-www-form-urlencoded'];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>['petstore_auth'];

    if (
      nullableContentType != null &&
      nullableContentType.toLowerCase().startsWith('multipart/form-data')
    ) {
      bool hasFields = false;
      final mp = MultipartRequest(null, null);
      if (name != null) {
        hasFields = true;
        mp.fields['name'] = parameterToString(name);
      }
      if (status != null) {
        hasFields = true;
        mp.fields['status'] = parameterToString(status);
      }
      if (hasFields) {
        postBody = mp;
      }
    } else {
      if (name != null) {
        formParams['name'] = parameterToString(name);
      }
      if (status != null) {
        formParams['status'] = parameterToString(status);
      }
    }

    return await apiClient.streamAPI(
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

  /// Executes the same logic as [updatePetWithForm] but instead returns the processed result as
  /// an HTTP [Response].
  Future<Response> updatePetWithFormWithHttpInfo(int petId, { String name, String status }) =>
    apiClient.getResponse(updatePetWithFormAsStreamedResponse(petId,  name: name, status: status ));

  /// Updates a pet in the store with form data
  ///
  /// Note: This method returns nothing.
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
      throw ApiException(response.statusCode, _decodeBodyBytes(response));
    }
  }

  /// Executes the same logic as [uploadFile] but instead returns a
  /// [StreamedResponse] that clients can listen to, for example to monitor the progress of
  /// the network activity.
  Future<StreamedResponse> uploadFileAsStreamedResponse(int petId, { String additionalMetadata, MultipartFile file }) async {
    // Verify required params are set.
    if (petId == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: petId');
    }

    final path = '/pet/{petId}/uploadImage'.replaceAll('{format}', 'json')
      .replaceAll('{' + 'petId' + '}', petId.toString());

    Object postBody;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};


    final contentTypes = <String>['multipart/form-data'];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>['petstore_auth'];

    if (
      nullableContentType != null &&
      nullableContentType.toLowerCase().startsWith('multipart/form-data')
    ) {
      bool hasFields = false;
      final mp = MultipartRequest(null, null);
      if (additionalMetadata != null) {
        hasFields = true;
        mp.fields['additionalMetadata'] = parameterToString(additionalMetadata);
      }
      if (file != null) {
        hasFields = true;
        mp.fields['file'] = file.field;
        mp.files.add(file);
      }
      if (hasFields) {
        postBody = mp;
      }
    } else {
      if (additionalMetadata != null) {
        formParams['additionalMetadata'] = parameterToString(additionalMetadata);
      }
    }

    return await apiClient.streamAPI(
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

  /// Executes the same logic as [uploadFile] but instead returns the processed result as
  /// an HTTP [Response].
  Future<Response> uploadFileWithHttpInfo(int petId, { String additionalMetadata, MultipartFile file }) =>
    apiClient.getResponse(uploadFileAsStreamedResponse(petId,  additionalMetadata: additionalMetadata, file: file ));

  /// uploads an image
  ///
  /// After the method completes, the result is returned as an instance of [ApiResponse].
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
      throw ApiException(response.statusCode, _decodeBodyBytes(response));
    }
    // When a remote server returns no body with a status of 204, we shall not decode it.
    // At the time of writing this, `dart:convert` will throw an "Unexpected end of input"
    // FormatException when trying to decode an empty string.
    if (response.body != null && response.statusCode != HttpStatus.noContent) {
      return apiClient.deserialize(_decodeBodyBytes(response), 'ApiResponse') as ApiResponse;
    }
    return null;
  }
}
