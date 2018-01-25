part of swagger.api;

class ApiKeyAuth implements Authentication {

  final String location;
  final String paramName;
  String apiKey;
  String apiKeyPrefix;

  ApiKeyAuth(this.location, this.paramName);

  @override
  void applyToParams(List<QueryParam> queryParams, Map<String, String> headerParams) {
    String value;
    if (apiKeyPrefix != null) {
      value = '$apiKeyPrefix $apiKey';
    } else {
      value = apiKey;
    }

    if (location == 'query' && value != null) {
      queryParams.add(new QueryParam(paramName, value));
    } else if (location == 'header' && value != null) {
      headerParams[paramName] = value;
    }
  }
}
