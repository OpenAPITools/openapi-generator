//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/deprecated_object.dart';
import 'package:freezed_annotation/freezed_annotation.dart';

part 'object_with_deprecated_fields.freezed.dart';
part 'object_with_deprecated_fields.g.dart';


@freezed
abstract class ObjectWithDeprecatedFields with _$ObjectWithDeprecatedFields {
  const factory ObjectWithDeprecatedFields({
    @JsonKey(
      name: r'uuid',
      includeIfNull: false,
    )
    String? uuid,
    @JsonKey(
      name: r'id',
      includeIfNull: false,
    )
    num? id,
    @JsonKey(
      name: r'deprecatedRef',
      includeIfNull: false,
    )
    DeprecatedObject? deprecatedRef,
    @JsonKey(
      name: r'bars',
      includeIfNull: false,
    )
    List<String>? bars,
  }) = _ObjectWithDeprecatedFields;

  factory ObjectWithDeprecatedFields.fromJson(Map<String, dynamic> json) =>
      _$ObjectWithDeprecatedFieldsFromJson(json);
}

