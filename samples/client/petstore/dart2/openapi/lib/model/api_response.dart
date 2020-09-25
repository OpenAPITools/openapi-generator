part of openapi.api;

class ApiResponse {
  
  int code = null;
  
  String type = null;
  
  String message = null;
  ApiResponse();

  @override
  String toString() {
    return 'ApiResponse[code=$code, type=$type, message=$message, ]';
  }

  ApiResponse.fromJson(Map<String, dynamic> json) {
    if (json == null) return;
    code = json['code'];
    type = json['type'];
    message = json['message'];
  }

  Map<String, dynamic> toJson() {
    Map <String, dynamic> json = {};
    if (code != null)
      json['code'] = code;
    if (type != null)
      json['type'] = type;
    if (message != null)
      json['message'] = message;
    return json;
  }

  static List<ApiResponse> listFromJson(List<dynamic> json) {
    return json == null ? List<ApiResponse>() : json.map((value) => ApiResponse.fromJson(value)).toList();
  }

  static Map<String, ApiResponse> mapFromJson(Map<String, dynamic> json) {
    var map = Map<String, ApiResponse>();
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic value) => map[key] = ApiResponse.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of ApiResponse-objects as value to a dart map
  static Map<String, List<ApiResponse>> mapListFromJson(Map<String, dynamic> json) {
    var map = Map<String, List<ApiResponse>>();
     if (json != null && json.isNotEmpty) {
       json.forEach((String key, dynamic value) {
         map[key] = ApiResponse.listFromJson(value);
       });
     }
     return map;
  }
}

