part of api;

class ApiKeyAuth implements Authentication {

  final String location;
  final String paramName;
  String apiKey;
  String apiKeyPrefix;

  ApiKeyAuth(this.location, this.paramName);

  @override
  void applyToParams(Map<String, String> queryParams, Map<String, String> headerParams) {
    String value;
    if (apiKeyPrefix != null) {
      value = '$apiKeyPrefix $apiKey';
    } else {
      value = apiKey;
    }

    if (location == 'query' && value != null) {
      queryParams[paramName] = value;
    } else if (location == 'header' && value != null) {
      headerParams[paramName] = value;
    }
  }

}