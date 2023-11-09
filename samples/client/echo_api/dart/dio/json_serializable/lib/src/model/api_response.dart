//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';

part 'api_response.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
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
    required: false,
    includeIfNull: false
  )


  final int? code;



  @JsonKey(
    
    name: r'type',
    required: false,
    includeIfNull: false
  )


  final String? type;



  @JsonKey(
    
    name: r'message',
    required: false,
    includeIfNull: false
  )


  final String? message;



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

  factory ApiResponse.fromJson(Map<String, dynamic> json) => _$ApiResponseFromJson(json);

  Map<String, dynamic> toJson() => _$ApiResponseToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

