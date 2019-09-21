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
    if (json['code'] == null) {
      code = null;
    } else {
      code = json['code'];
    }
    if (json['type'] == null) {
      type = null;
    } else {
      type = json['type'];
    }
    if (json['message'] == null) {
      message = null;
    } else {
      message = json['message'];
    }
  }

  Map<String, dynamic> toJson() {
    return {
      'code': code,
      'type': type,
      'message': message
    };
  }

  static List<ApiResponse> listFromJson(List<dynamic> json) {
    return json == null ? new List<ApiResponse>() : json.map((value) => new ApiResponse.fromJson(value)).toList();
  }

  static Map<String, ApiResponse> mapFromJson(Map<String, Map<String, dynamic>> json) {
    var map = new Map<String, ApiResponse>();
    if (json != null && json.length > 0) {
      json.forEach((String key, Map<String, dynamic> value) => map[key] = new ApiResponse.fromJson(value));
    }
    return map;
  }
}

