part of openapi.api;

class HttpBasicAuth implements Authentication {

  String _username;
  String _password;

  @override
  void applyToParams(List<QueryParam> queryParams, Map<String, String> headerParams) {
    String str = (_username == null ? "" : _username) + ":" + (_password == null ? "" : _password);
    headerParams["Authorization"] = "Basic " + base64.encode(utf8.encode(str));
  }

  set username(String username) => _username = username;
  set password(String password) => _password = password;
}
