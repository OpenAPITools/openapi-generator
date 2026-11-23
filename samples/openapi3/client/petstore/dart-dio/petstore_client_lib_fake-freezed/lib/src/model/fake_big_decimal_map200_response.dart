//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:freezed_annotation/freezed_annotation.dart';

part 'fake_big_decimal_map200_response.freezed.dart';
part 'fake_big_decimal_map200_response.g.dart';


@freezed
abstract class FakeBigDecimalMap200Response with _$FakeBigDecimalMap200Response {
  const factory FakeBigDecimalMap200Response({
    @JsonKey(
      name: r'someId',
      required: false,
      includeIfNull: false,
    )
    num? someId,
    @JsonKey(
      name: r'someMap',
      required: false,
      includeIfNull: false,
    )
    Map<String, num>? someMap,
  }) = _FakeBigDecimalMap200Response;

  factory FakeBigDecimalMap200Response.fromJson(Map<String, dynamic> json) =>
      _$FakeBigDecimalMap200ResponseFromJson(json);
}

