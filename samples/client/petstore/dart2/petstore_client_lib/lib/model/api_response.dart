//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

@JsonSerializable(
  checked: true,
  includeIfNull: false,
  disallowUnrecognizedKeys: true,
)
class ApiResponse {
  /// Returns a new [ApiResponse] instance.
  ApiResponse({
    this.code,
    this.type,
    this.message,
  });

  @JsonKey(
    name: r'code',
    
    
    
  )
  int code;

  @JsonKey(
    name: r'type',
    
    
    
  )
  String type;

  @JsonKey(
    name: r'message',
    
    
    
  )
  String message;

  @override
  bool operator ==(Object other) => identical(this, other) || other is ApiResponse &&
     other.code == code &&
     other.type == type &&
     other.message == message;

  @override
  int get hashCode =>
    (code == null ? 0 : code.hashCode) +
    (type == null ? 0 : type.hashCode) +
    (message == null ? 0 : message.hashCode);

  @override
  String toString() => toJson().toString();

  factory ApiResponse.fromJson(Map<String, dynamic> json) => _$ApiResponseFromJson(json);
  Map<String, dynamic> toJson() => _$ApiResponseToJson(this);
}

