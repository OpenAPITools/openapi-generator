//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:copy_with_extension/copy_with_extension.dart';
import 'package:json_annotation/json_annotation.dart';

part 'object_with_inline_enum.g.dart';


@CopyWith()
@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class ObjectWithInlineEnum {
  /// Returns a new [ObjectWithInlineEnum] instance.
  ObjectWithInlineEnum({

     this.attribute,
  });

      /// Object one attribute enum
  @JsonKey(
    
    name: r'attribute',
    required: false,
    includeIfNull: false,
  unknownEnumValue: Set<ObjectWithInlineEnumAttributeEnum>.unknownDefaultOpenApi,
  )


  final Set<ObjectWithInlineEnumAttributeEnum>? attribute;





    @override
    bool operator ==(Object other) => identical(this, other) || other is ObjectWithInlineEnum &&
      other.attribute == attribute;

    @override
    int get hashCode =>
        attribute.hashCode;

  factory ObjectWithInlineEnum.fromJson(Map<String, dynamic> json) => _$ObjectWithInlineEnumFromJson(json);

  Map<String, dynamic> toJson() => _$ObjectWithInlineEnumToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

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


