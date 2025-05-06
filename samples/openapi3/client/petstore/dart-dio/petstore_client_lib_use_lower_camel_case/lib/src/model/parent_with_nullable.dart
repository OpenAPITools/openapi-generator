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

     this.nullableproperty,
  });

  @JsonKey(
    
    name: r'type',
    required: false,
    includeIfNull: false,
  unknownEnumValue: ParentWithNullableTypeEnum.unknownDefaultOpenApi,
  )


  final ParentWithNullableTypeEnum? type;



  @JsonKey(
    
    name: r'nullableProperty',
    required: false,
    includeIfNull: false,
  )


  final String? nullableproperty;





    @override
    bool operator ==(Object other) => identical(this, other) || other is ParentWithNullable &&
      other.type == type &&
      other.nullableproperty == nullableproperty;

    @override
    int get hashCode =>
        type.hashCode +
        (nullableproperty == null ? 0 : nullableproperty.hashCode);

  factory ParentWithNullable.fromJson(Map<String, dynamic> json) => _$ParentWithNullableFromJson(json);

  Map<String, dynamic> toJson() => _$ParentWithNullableToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}


enum ParentWithNullableTypeEnum {
@JsonValue(r'ChildWithNullable')
childwithnullable(r'ChildWithNullable'),
@JsonValue(r'unknown_default_open_api')
unknownDefaultOpenApi(r'unknown_default_open_api');

const ParentWithNullableTypeEnum(this.value);

final String value;

@override
String toString() => value;
}


