//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:freezed_annotation/freezed_annotation.dart';

part 'api_response.freezed.dart';
part 'api_response.g.dart';


@freezed
abstract class ApiResponse with _$ApiResponse {
  const factory ApiResponse({
    @JsonKey(
      name: r'code',
      includeIfNull: false,
    )
    int? code,
    @JsonKey(
      name: r'type',
      includeIfNull: false,
    )
    String? type,
    @JsonKey(
      name: r'message',
      includeIfNull: false,
    )
    String? message,
  }) = _ApiResponse;

  factory ApiResponse.fromJson(Map<String, dynamic> json) =>
      _$ApiResponseFromJson(json);
}

