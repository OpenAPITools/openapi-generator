//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.14

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class ApiResponse {
  /// Returns a new [ApiResponse] instance.
  ApiResponse({
    this.code,
    this.type,
    this.message,
  });


  int? code;

  String? type;

  String? message;

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
  String toString() => 'ApiResponse[code=$code, type=$type, message=$message]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (code != null) {
      json[r'code'] = code;
    }
    if (type != null) {
      json[r'type'] = type;
    }
    if (message != null) {
      json[r'message'] = message;
    }
    return json;
  }

  /// Returns a new [ApiResponse] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static ApiResponse fromJson(Map<String, dynamic> json) => ApiResponse(
        code: json[r'code'] as int,
        type: json[r'type'] as String,
        message: json[r'message'] as String,
    );

  static List<ApiResponse> listFromJson(List json, {bool? growable,}) =>
    json.isNotEmpty
      ? json.map<ApiResponse>((i) => ApiResponse.fromJson(i as Map<String, dynamic>)).toList(growable: true == growable)
      : <ApiResponse>[];

  static Map<String, ApiResponse> mapFromJson(dynamic json) {
    final map = <String, ApiResponse>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) => map[key] = ApiResponse.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of ApiResponse-objects as value to a dart map
  static Map<String, List<ApiResponse>> mapListFromJson(dynamic json, {bool? growable,}) {
    final map = <String, List<ApiResponse>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = ApiResponse.listFromJson(
            value,
            growable: growable,
          );
        });
    }
    return map;
  }
}

