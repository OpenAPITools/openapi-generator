part of api;

abstract class Authentication {

  void applyToParams(Map<String, String> queryParams, Map<String, String> headerParams);
}