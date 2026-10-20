//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:freezed_annotation/freezed_annotation.dart';

part 'model200_response.freezed.dart';
part 'model200_response.g.dart';


@freezed
abstract class Model200Response with _$Model200Response {
  const factory Model200Response({
    @JsonKey(
      name: r'name',
      required: false,
      includeIfNull: false,
    )
    int? name,
    @JsonKey(
      name: r'class',
      required: false,
      includeIfNull: false,
    )
    String? class_,
  }) = _Model200Response;

  factory Model200Response.fromJson(Map<String, dynamic> json) =>
      _$Model200ResponseFromJson(json);
}

