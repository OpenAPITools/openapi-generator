//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;


class AnotherFakeApi {
  AnotherFakeApi([ApiClient apiClient]) : apiClient = apiClient ?? defaultApiClient;

  final ApiClient apiClient;

  /// To test special tags
  ///
  /// To test special tags and operation ID starting with number
  ///
  /// Note: This method returns the HTTP [Response].
  ///
  /// Parameters:
  ///
  /// * [ModelClient] modelClient (required):
  ///   client model
  Future<Response> call123testSpecialTagsWithHttpInfo(ModelClient modelClient,) async {
    // Verify required params are set.
    if (modelClient == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: modelClient');
    }

    // ignore: prefer_const_declarations
    final path = r'/another-fake/dummy';

    // ignore: prefer_final_locals
    Object postBody = modelClient;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    const authNames = <String>[];
    const contentTypes = <String>['application/json'];


    return apiClient.invokeAPI(
      path,
      'PATCH',
      queryParams,
      postBody,
      headerParams,
      formParams,
      contentTypes.isEmpty ? null : contentTypes[0],
      authNames,
    );
  }

  /// To test special tags
  ///
  /// To test special tags and operation ID starting with number
  ///
  /// Parameters:
  ///
  /// * [ModelClient] modelClient (required):
  ///   client model
  Future<ModelClient> call123testSpecialTags(ModelClient modelClient,) async {
    final response = await call123testSpecialTagsWithHttpInfo(modelClient,);
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
    // When a remote server returns no body with a status of 204, we shall not decode it.
    // At the time of writing this, `dart:convert` will throw an "Unexpected end of input"
    // FormatException when trying to decode an empty string.
    if (response.body != null && response.statusCode != HttpStatus.noContent) {
      return await apiClient.deserializeAsync(await _decodeBodyBytes(response), 'ModelClient',) as ModelClient;
    
    }
    return Future<ModelClient>.value();
  }
}
