//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:freezed_annotation/freezed_annotation.dart';

part 'object_with_inline_enum_default_value.freezed.dart';
part 'object_with_inline_enum_default_value.g.dart';


@freezed
abstract class ObjectWithInlineEnumDefaultValue with _$ObjectWithInlineEnumDefaultValue {
  const factory ObjectWithInlineEnumDefaultValue({
    /// Object one attribute enum with default value
    @JsonKey(
      name: r'attribute',
      required: false,
      includeIfNull: false,
      unknownEnumValue: ObjectWithInlineEnumDefaultValueAttributeEnum.unknownDefaultOpenApi,
    )
  
    @Default(ObjectWithInlineEnumDefaultValueAttributeEnum.valueOne) ObjectWithInlineEnumDefaultValueAttributeEnum attribute,
  
  }) = _ObjectWithInlineEnumDefaultValue;

  factory ObjectWithInlineEnumDefaultValue.fromJson(Map<String, dynamic> json) =>
      _$ObjectWithInlineEnumDefaultValueFromJson(json);
}

/// Object one attribute enum with default value
enum ObjectWithInlineEnumDefaultValueAttributeEnum {
@JsonValue(r'value_one')
valueOne(r'value_one'),
@JsonValue(r'value_two')
valueTwo(r'value_two'),
@JsonValue(r'unknown_default_open_api')
unknownDefaultOpenApi(r'unknown_default_open_api');

const ObjectWithInlineEnumDefaultValueAttributeEnum(this.value);

final String value;

@override
String toString() => value;
}


