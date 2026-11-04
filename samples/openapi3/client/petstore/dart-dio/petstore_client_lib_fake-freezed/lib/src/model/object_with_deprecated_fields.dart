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
      required: false,
      includeIfNull: false,
    )
    String? uuid,
    @Deprecated('id has been deprecated')
    @JsonKey(
      name: r'id',
      required: false,
      includeIfNull: false,
    )
    num? id,
    @Deprecated('deprecatedRef has been deprecated')
    @JsonKey(
      name: r'deprecatedRef',
      required: false,
      includeIfNull: false,
    )
    DeprecatedObject? deprecatedRef,
    @Deprecated('bars has been deprecated')
    @JsonKey(
      name: r'bars',
      required: false,
      includeIfNull: false,
    )
    List<String>? bars,
  }) = _ObjectWithDeprecatedFields;

  factory ObjectWithDeprecatedFields.fromJson(Map<String, dynamic> json) =>
      _$ObjectWithDeprecatedFieldsFromJson(json);
}

