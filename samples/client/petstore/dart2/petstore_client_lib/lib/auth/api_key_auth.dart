//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class ApiKeyAuth implements Authentication {
  ApiKeyAuth(this.location, this.paramName);

  final String location;
  final String paramName;

  String apiKeyPrefix;
  String apiKey;

  @override
  void applyToParams(List<QueryParam> queryParams, Map<String, String> headerParams) {
    final value = apiKeyPrefix == null ? apiKey : '$apiKeyPrefix $apiKey';

    if (location == 'query' && value != null) {
      queryParams.add(QueryParam(paramName, value));
    } else if (location == 'header' && value != null) {
      headerParams[paramName] = value;
    } else if (location == 'cookie' && value != null) {
      headerParams.update('Cookie', (String existingCookie) {
        return '$existingCookie; $paramName=$value';
      }, ifAbsent: () => '$paramName=$value');
    }
  }
}
