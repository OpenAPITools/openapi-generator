//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element
// ignore_for_file: always_put_required_named_parameters_first

part of openapi.api;

class ApiResponse {
  ApiResponse({
    this.code,
    this.type,
    this.message,
  });

  
  int code;

  
  String type;

  
  String message;

  @override
  String toString() => 'ApiResponse[code=$code, type=$type, message=$message, ]';

  ApiResponse.fromJson(Map<String, dynamic> json) {
    if (json == null) return;
    code = json['code'];
    type = json['type'];
    message = json['message'];
  }

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (code != null)
      json['code'] = code;
    if (type != null)
      json['type'] = type;
    if (message != null)
      json['message'] = message;
    return json;
  }

  static List<ApiResponse> listFromJson(List<dynamic> json, {bool growable}) =>
    json == null
      ? <ApiResponse>[]
      : json.map((v) => ApiResponse.fromJson(v)).toList(growable: true == growable);

  static Map<String, ApiResponse> mapFromJson(Map<String, dynamic> json) {
    final map = <String, ApiResponse>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) => map[key] = ApiResponse.fromJson(v));
    }
    return map;
  }

  // maps a json object with a list of ApiResponse-objects as value to a dart map
  static Map<String, List<ApiResponse>> mapListFromJson(Map<String, dynamic> json, {bool growable}) {
    final map = <String, List<ApiResponse>>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) {
        map[key] = ApiResponse.listFromJson(v, growable: growable);
      });
    }
    return map;
  }
}

