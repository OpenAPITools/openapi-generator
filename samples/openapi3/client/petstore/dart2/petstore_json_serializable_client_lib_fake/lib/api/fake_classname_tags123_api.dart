//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;


class FakeClassnameTags123Api {
  FakeClassnameTags123Api([ApiClient apiClient]) : apiClient = apiClient ?? defaultApiClient;

  final ApiClient apiClient;

  /// To test class name in snake case
  ///
  /// To test class name in snake case
  ///
  /// Note: This method returns the HTTP [Response].
  ///
  /// Parameters:
  ///
  /// * [ModelClient] modelClient (required):
  ///   client model
  Future<Response> testClassnameWithHttpInfo(ModelClient modelClient) async {
    // Verify required params are set.
    if (modelClient == null) {
     throw ApiException(HttpStatus.badRequest, 'Missing required param: modelClient');
    }

    final path = r'/fake_classname_test';

    Object postBody = modelClient;

    final queryParams = <QueryParam>[];
    final headerParams = <String, String>{};
    final formParams = <String, String>{};

    final contentTypes = <String>['application/json'];
    final nullableContentType = contentTypes.isNotEmpty ? contentTypes[0] : null;
    final authNames = <String>['api_key_query'];


    return await apiClient.invokeAPI(
      path,
      'PATCH',
      queryParams,
      postBody,
      headerParams,
      formParams,
      nullableContentType,
      authNames,
    );
  }

  /// To test class name in snake case
  ///
  /// To test class name in snake case
  ///
  /// Parameters:
  ///
  /// * [ModelClient] modelClient (required):
  ///   client model
  Future<ModelClient> testClassname(ModelClient modelClient) async {
    final response = await testClassnameWithHttpInfo(modelClient);
    if (response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(response.statusCode, await _decodeBodyBytes(response));
    }
    // When a remote server returns no body with a status of 204, we shall not decode it.
    // At the time of writing this, `dart:convert` will throw an "Unexpected end of input"
    // FormatException when trying to decode an empty string.
    if (response.body != null && response.statusCode != HttpStatus.noContent) {

      return ModelClient.fromJson(json.decode(response.body));
    }
    return Future<ModelClient>.value(null);
  }
}
