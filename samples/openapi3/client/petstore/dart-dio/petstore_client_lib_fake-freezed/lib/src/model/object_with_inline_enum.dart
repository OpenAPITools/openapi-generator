//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:freezed_annotation/freezed_annotation.dart';

part 'object_with_inline_enum.freezed.dart';
part 'object_with_inline_enum.g.dart';


@freezed
abstract class ObjectWithInlineEnum with _$ObjectWithInlineEnum {
  const factory ObjectWithInlineEnum({
    /// Object one attribute enum
    @JsonKey(
      name: r'attribute',
      required: false,
      includeIfNull: false,
      unknownEnumValue: ObjectWithInlineEnumAttributeEnum.unknownDefaultOpenApi,
    )
    Set<ObjectWithInlineEnumAttributeEnum>? attribute,
  }) = _ObjectWithInlineEnum;

  factory ObjectWithInlineEnum.fromJson(Map<String, dynamic> json) =>
      _$ObjectWithInlineEnumFromJson(json);
}

enum ObjectWithInlineEnumAttributeEnum {
@JsonValue(r'value_one')
valueOne(r'value_one'),
@JsonValue(r'value_two')
valueTwo(r'value_two'),
@JsonValue(r'unknown_default_open_api')
unknownDefaultOpenApi(r'unknown_default_open_api');

const ObjectWithInlineEnumAttributeEnum(this.value);

final String value;

@override
String toString() => value;
}


