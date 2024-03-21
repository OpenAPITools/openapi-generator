//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.18

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;


class FakeClassnameTags123Api {
  FakeClassnameTags123Api([ApiClient? apiClient]) : apiClient = apiClient ?? defaultApiClient;

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
  Future<Response> testClassnameWithHttpInfo(ModelClient modelClient,) async {
    // ignore: prefer_const_declarations
    final _path = r'/fake_classname_test';

    // ignore: prefer_final_locals
    Object? _postBody = modelClient;

    final __queryParams = <QueryParam>[];
    final _headerParams = <String, String>{};
    final _formParams = <String, String>{};

    const _contentTypes = <String>['application/json'];


    return apiClient.invokeAPI(
      _path,
      'PATCH',
      __queryParams,
      _postBody,
      _headerParams,
      _formParams,
      _contentTypes.isEmpty ? null : _contentTypes.first,
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
  Future<ModelClient?> testClassname(ModelClient modelClient,) async {
    final _response = await testClassnameWithHttpInfo(modelClient,);
    if (_response.statusCode >= HttpStatus.badRequest) {
      throw ApiException(_response.statusCode, await _decodeBodyBytes(_response));
    }
    // When a remote server returns no body with a status of 204, we shall not decode it.
    // At the time of writing this, `dart:convert` will throw an "Unexpected end of input"
    // FormatException when trying to decode an empty string.
    if (_response.bodyBytes.isNotEmpty && _response.statusCode != HttpStatus.noContent) {
      return await apiClient.deserializeAsync(await _decodeBodyBytes(_response), 'ModelClient',) as ModelClient;
    
    }
    return null;
  }
}
