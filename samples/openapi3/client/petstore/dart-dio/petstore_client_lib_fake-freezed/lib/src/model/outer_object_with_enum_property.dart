//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/outer_enum_integer.dart';
import 'package:freezed_annotation/freezed_annotation.dart';

part 'outer_object_with_enum_property.freezed.dart';
part 'outer_object_with_enum_property.g.dart';


@freezed
abstract class OuterObjectWithEnumProperty with _$OuterObjectWithEnumProperty {
  const factory OuterObjectWithEnumProperty({
    @JsonKey(
      name: r'value',
      required: true,
      includeIfNull: false,
      unknownEnumValue: OuterEnumInteger.unknownDefaultOpenApi,
    )
    required OuterEnumInteger value,
  }) = _OuterObjectWithEnumProperty;

  factory OuterObjectWithEnumProperty.fromJson(Map<String, dynamic> json) =>
      _$OuterObjectWithEnumPropertyFromJson(json);
}

