//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:copy_with_extension/copy_with_extension.dart';
import 'package:json_annotation/json_annotation.dart';

part 'object_with_duplicate_inline_enum.g.dart';


@CopyWith()
@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class ObjectWithDuplicateInlineEnum {
  /// Returns a new [ObjectWithDuplicateInlineEnum] instance.
  ObjectWithDuplicateInlineEnum({

     this.attribute,
  });

      /// Object two attribute enum
  @JsonKey(
    
    name: r'attribute',
    required: false,
    includeIfNull: false,
  unknownEnumValue: Set<ObjectWithDuplicateInlineEnumAttributeEnum>.unknownDefaultOpenApi,
  )


  final Set<ObjectWithDuplicateInlineEnumAttributeEnum>? attribute;





    @override
    bool operator ==(Object other) => identical(this, other) || other is ObjectWithDuplicateInlineEnum &&
      other.attribute == attribute;

    @override
    int get hashCode =>
        attribute.hashCode;

  factory ObjectWithDuplicateInlineEnum.fromJson(Map<String, dynamic> json) => _$ObjectWithDuplicateInlineEnumFromJson(json);

  Map<String, dynamic> toJson() => _$ObjectWithDuplicateInlineEnumToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

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


