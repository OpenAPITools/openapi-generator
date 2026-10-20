//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/test_enum.dart';
import 'package:copy_with_extension/copy_with_extension.dart';
import 'package:json_annotation/json_annotation.dart';

part 'object_with_enum.g.dart';


@CopyWith()
@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class ObjectWithEnum {
  /// Returns a new [ObjectWithEnum] instance.
  ObjectWithEnum({

     this.attribute = TestEnum.empty,
  });

  @JsonKey(
    defaultValue: TestEnum.empty,
    name: r'attribute',
    required: false,
    includeIfNull: false,
  unknownEnumValue: TestEnum.unknownDefaultOpenApi,
  )


  final TestEnum? attribute;





    @override
    bool operator ==(Object other) => identical(this, other) || other is ObjectWithEnum &&
      other.attribute == attribute;

    @override
    int get hashCode =>
        attribute.hashCode;

  factory ObjectWithEnum.fromJson(Map<String, dynamic> json) => _$ObjectWithEnumFromJson(json);

  Map<String, dynamic> toJson() => _$ObjectWithEnumToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

