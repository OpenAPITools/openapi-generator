part of openapi.api;

class ApiKeyAuth implements Authentication {

  final String location;
  final String paramName;
  String _apiKey;
  String apiKeyPrefix;

  set apiKey(String key) => _apiKey = key;

  ApiKeyAuth(this.location, this.paramName);

  @override
  void applyToParams(List<QueryParam> queryParams, Map<String, String> headerParams) {
    String value;
    if (apiKeyPrefix != null) {
      value = '$apiKeyPrefix $_apiKey';
    } else {
      value = _apiKey;
    }

    if (location == 'query' && value != null) {
      queryParams.add(QueryParam(paramName, value));
    } else if (location == 'header' && value != null) {
      headerParams[paramName] = value;
    } else if (location == 'cookie' && value != null) {
      headerParams.update('Cookie', (String existingCookie) {
        return "$existingCookie; $paramName=$value";
      }, ifAbsent: () => '$paramName=$value');
    }
  }
}
