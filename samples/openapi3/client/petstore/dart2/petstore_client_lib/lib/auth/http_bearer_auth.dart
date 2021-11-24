//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

typedef HttpBearerAuthProvider = String Function();

class HttpBearerAuth implements Authentication {
  HttpBearerAuth();

  dynamic _accessToken;

  dynamic get accessToken => _accessToken;

  set accessToken(dynamic accessToken) {
    if (accessToken is! String && accessToken is! HttpBearerAuthProvider) {
      throw ArgumentError('Type of Bearer accessToken should be a String or a String Function().');
    }
    _accessToken = accessToken;
  }

  @override
  void applyToParams(List<QueryParam> queryParams, Map<String, String> headerParams) {
    if (_accessToken is String) {
      headerParams['Authorization'] = 'Bearer $_accessToken';
    } else if (_accessToken is HttpBearerAuthProvider) {
      headerParams['Authorization'] = 'Bearer ${_accessToken()}';
    } else {
      throw ArgumentError('Type of Bearer accessToken should be a String or a String Function().');
    }
  }
}
