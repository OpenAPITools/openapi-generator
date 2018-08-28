part of openapi.api;

class HttpBasicAuth implements Authentication {

  String username;
  String password;

  @override
  void applyToParams(List<QueryParam> queryParams, Map<String, String> headerParams) {
    String str = (username == null ? "" : username) + ":" + (password == null ? "" : password);
    headerParams["Authorization"] = "Basic " + base64.encode(utf8.encode(str));
  }

}
