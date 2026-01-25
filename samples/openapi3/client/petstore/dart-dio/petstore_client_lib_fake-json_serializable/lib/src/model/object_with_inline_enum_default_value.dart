//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:copy_with_extension/copy_with_extension.dart';
import 'package:json_annotation/json_annotation.dart';

part 'object_with_inline_enum_default_value.g.dart';


@CopyWith()
@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class ObjectWithInlineEnumDefaultValue {
  /// Returns a new [ObjectWithInlineEnumDefaultValue] instance.
  ObjectWithInlineEnumDefaultValue({

     this.attribute = const ObjectWithInlineEnumDefaultValueAttributeEnum._('value_one'),
  });

      /// Object one attribute enum with default value
  @JsonKey(
    defaultValue: 'value_one',
    name: r'attribute',
    required: false,
    includeIfNull: false,
  unknownEnumValue: ObjectWithInlineEnumDefaultValueAttributeEnum.unknownDefaultOpenApi,
  )


  final ObjectWithInlineEnumDefaultValueAttributeEnum? attribute;





    @override
    bool operator ==(Object other) => identical(this, other) || other is ObjectWithInlineEnumDefaultValue &&
      other.attribute == attribute;

    @override
    int get hashCode =>
        attribute.hashCode;

  factory ObjectWithInlineEnumDefaultValue.fromJson(Map<String, dynamic> json) => _$ObjectWithInlineEnumDefaultValueFromJson(json);

  Map<String, dynamic> toJson() => _$ObjectWithInlineEnumDefaultValueToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

/// Object one attribute enum with default value
enum ObjectWithInlineEnumDefaultValueAttributeEnum {
    /// Object one attribute enum with default value
@JsonValue(r'value_one')
valueOne(r'value_one'),
    /// Object one attribute enum with default value
@JsonValue(r'value_two')
valueTwo(r'value_two'),
    /// Object one attribute enum with default value
@JsonValue(r'unknown_default_open_api')
unknownDefaultOpenApi(r'unknown_default_open_api');

const ObjectWithInlineEnumDefaultValueAttributeEnum(this.value);

final String value;

@override
String toString() => value;
}


