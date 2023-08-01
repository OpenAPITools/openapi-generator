//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/outer_enum_integer.dart';
import 'package:json_annotation/json_annotation.dart';

part 'outer_object_with_enum_property.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class OuterObjectWithEnumProperty {
  /// Returns a new [OuterObjectWithEnumProperty] instance.
  OuterObjectWithEnumProperty({

    required  this.value,
  });

  @JsonKey(
    
    name: r'value',
    required: true,
    includeIfNull: false
  )


  final OuterEnumInteger value;



  @override
  bool operator ==(Object other) => identical(this, other) || other is OuterObjectWithEnumProperty &&
     other.value == value;

  @override
  int get hashCode =>
    value.hashCode;

  factory OuterObjectWithEnumProperty.fromJson(Map<String, dynamic> json) => _$OuterObjectWithEnumPropertyFromJson(json);

  Map<String, dynamic> toJson() => _$OuterObjectWithEnumPropertyToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

