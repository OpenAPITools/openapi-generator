//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: avoid_init_to_null, lines_longer_than_80_chars
// ignore_for_file: prefer_single_quotes

part of openapi.api;

/// [String] values for all properties defined in [ApiResponse].
abstract class ApiResponseStrings {
  const ApiResponseStrings._();

  static const code_ = "code";
  static const type_ = "type";
  static const message_ = "message";
}

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
  bool operator ==(Object other) => identical(this, other) || other is ApiResponse &&
     other.code == code &&
     other.type == type &&
     other.message == message;

  @override
  int get hashCode =>
    code.hashCode +
    type.hashCode +
    message.hashCode;

  @override
  String toString() => _toString("");

  ApiResponse.fromJson(Map<String, dynamic> json) {
    if (json == null) {
      return;
    }
    code = json[ApiResponseStrings.code_];
    type = json[ApiResponseStrings.type_];
    message = json[ApiResponseStrings.message_];
  }

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (code != null) {
      json[ApiResponseStrings.code_] = code;
    }
    if (type != null) {
      json[ApiResponseStrings.type_] = type;
    }
    if (message != null) {
      json[ApiResponseStrings.message_] = message;
    }
    return json;
  }

  String _toString(String prefix) {
    final sb = StringBuffer();

    sb.write("ApiResponse=[");

    sb.write("\n$prefix  ");
    sb.write(ApiResponseStrings.code_);
    sb.write(": ");
    sb.write(code);
  sb.write(",");

    sb.write("\n$prefix  ");
    sb.write(ApiResponseStrings.type_);
    sb.write(": ");
    sb.write(type);
  sb.write(",");

    sb.write("\n$prefix  ");
    sb.write(ApiResponseStrings.message_);
    sb.write(": ");
    sb.write(message);
  

    sb.write("\n$prefix]");

    return sb.toString();
  }

  static List<ApiResponse> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <ApiResponse>[]
      : json.map((v) => ApiResponse.fromJson(v)).toList(growable: true == growable);

  static Map<String, ApiResponse> mapFromJson(Map<String, dynamic> json) {
    final map = <String, ApiResponse>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) => map[key] = ApiResponse.fromJson(v));
    }
    return map;
  }

  // maps a json object with a list of ApiResponse-objects as value to a dart map
  static Map<String, List<ApiResponse>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable}) {
    final map = <String, List<ApiResponse>>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) {
        map[key] = ApiResponse.listFromJson(v, emptyIsNull: emptyIsNull, growable: growable);
      });
    }
    return map;
  }
}

