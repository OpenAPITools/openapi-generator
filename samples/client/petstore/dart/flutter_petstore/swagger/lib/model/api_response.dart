part of swagger.api;

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
    code =
    json['code'];
    type =
    json['type'];
    message =
    json['message'];
  }

  Map<String, dynamic> toJson() {
    return {
      'code': code,
      'type': type,
      'message': message
     };
  }

  static List<ApiResponse> listFromJson(List<Map<String, dynamic>> json) {
    var list = new List<ApiResponse>();
    if (json != null && json.length > 0) {
      json.forEach((Map<String, dynamic> value) => list.add(new ApiResponse.fromJson(value)));
    }
    return list;
  }

  static Map<String, ApiResponse> mapFromJson(Map<String, Map<String, dynamic>> json) {
    var map = new Map<String, ApiResponse>();
    if (json != null && json.length > 0) {
      json.forEach((String key, Map<String, dynamic> value) => map[key] = new ApiResponse.fromJson(value));
    }
    return map;
  }
}

