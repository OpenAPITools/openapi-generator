//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';

part 'parent_with_nullable.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class ParentWithNullable {
  /// Returns a new [ParentWithNullable] instance.
  ParentWithNullable({

     this.type,

     this.nullableProperty,
  });

  @JsonKey(
    
    name: r'type',
    required: false,
    includeIfNull: false
  )


  final ParentWithNullableTypeEnum? type;



  @JsonKey(
    
    name: r'nullableProperty',
    required: false,
    includeIfNull: false
  )


  final String? nullableProperty;



  @override
  bool operator ==(Object other) => identical(this, other) || other is ParentWithNullable &&
     other.type == type &&
     other.nullableProperty == nullableProperty;

  @override
  int get hashCode =>
    type.hashCode +
    (nullableProperty == null ? 0 : nullableProperty.hashCode);

  factory ParentWithNullable.fromJson(Map<String, dynamic> json) => _$ParentWithNullableFromJson(json);

  Map<String, dynamic> toJson() => _$ParentWithNullableToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}


enum ParentWithNullableTypeEnum {
  @JsonValue(r'ChildWithNullable')
  childWithNullable,
  @JsonValue(r'unknown_default_open_api')
  unknownDefaultOpenApi,
}


