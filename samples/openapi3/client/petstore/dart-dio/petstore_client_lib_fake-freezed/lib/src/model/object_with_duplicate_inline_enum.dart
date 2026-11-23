//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:freezed_annotation/freezed_annotation.dart';

part 'object_with_duplicate_inline_enum.freezed.dart';
part 'object_with_duplicate_inline_enum.g.dart';


@freezed
abstract class ObjectWithDuplicateInlineEnum with _$ObjectWithDuplicateInlineEnum {
  const factory ObjectWithDuplicateInlineEnum({
    /// Object two attribute enum
    @JsonKey(
      name: r'attribute',
      required: false,
      includeIfNull: false,
      unknownEnumValue: ObjectWithDuplicateInlineEnumAttributeEnum.unknownDefaultOpenApi,
    )
    Set<ObjectWithDuplicateInlineEnumAttributeEnum>? attribute,
  }) = _ObjectWithDuplicateInlineEnum;

  factory ObjectWithDuplicateInlineEnum.fromJson(Map<String, dynamic> json) =>
      _$ObjectWithDuplicateInlineEnumFromJson(json);
}

enum ObjectWithDuplicateInlineEnumAttributeEnum {
@JsonValue(r'value_one')
valueOne(r'value_one'),
@JsonValue(r'value_two')
valueTwo(r'value_two'),
@JsonValue(r'unknown_default_open_api')
unknownDefaultOpenApi(r'unknown_default_open_api');

const ObjectWithDuplicateInlineEnumAttributeEnum(this.value);

final String value;

@override
String toString() => value;
}


