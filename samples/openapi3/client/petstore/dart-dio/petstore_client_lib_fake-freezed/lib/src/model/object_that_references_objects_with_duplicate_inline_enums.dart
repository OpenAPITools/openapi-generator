//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/object_with_duplicate_inline_enum.dart';
import 'package:openapi/src/model/object_with_inline_enum.dart';
import 'package:freezed_annotation/freezed_annotation.dart';

part 'object_that_references_objects_with_duplicate_inline_enums.freezed.dart';
part 'object_that_references_objects_with_duplicate_inline_enums.g.dart';


@freezed
abstract class ObjectThatReferencesObjectsWithDuplicateInlineEnums with _$ObjectThatReferencesObjectsWithDuplicateInlineEnums {
  const factory ObjectThatReferencesObjectsWithDuplicateInlineEnums({
    @JsonKey(
      name: r'object_one',
      required: false,
      includeIfNull: false,
    )
    ObjectWithInlineEnum? objectOne,
    @JsonKey(
      name: r'object_two',
      required: false,
      includeIfNull: false,
    )
    ObjectWithDuplicateInlineEnum? objectTwo,
  }) = _ObjectThatReferencesObjectsWithDuplicateInlineEnums;

  factory ObjectThatReferencesObjectsWithDuplicateInlineEnums.fromJson(Map<String, dynamic> json) =>
      _$ObjectThatReferencesObjectsWithDuplicateInlineEnumsFromJson(json);
}

