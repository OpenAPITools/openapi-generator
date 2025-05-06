//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/parent_with_nullable.dart';
import 'package:json_annotation/json_annotation.dart';

part 'child_with_nullable.g.dart';

// ignore_for_file: unused_import


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class ChildWithNullable {
  /// Returns a new [ChildWithNullable] instance.
  ChildWithNullable({

     this.type,

     this.nullableproperty,

     this.otherproperty,
  });

  @JsonKey(
    
    name: r'type',
    required: false,
    includeIfNull: false,
  unknownEnumValue: ChildWithNullableTypeEnum.unknownDefaultOpenApi,
  )


  final ChildWithNullableTypeEnum? type;



  @JsonKey(
    
    name: r'nullableProperty',
    required: false,
    includeIfNull: false,
  )


  final String? nullableproperty;



  @JsonKey(
    
    name: r'otherProperty',
    required: false,
    includeIfNull: false,
  )


  final String? otherproperty;





    @override
    bool operator ==(Object other) => identical(this, other) || other is ChildWithNullable &&
      other.type == type &&
      other.nullableproperty == nullableproperty &&
      other.otherproperty == otherproperty;

    @override
    int get hashCode =>
        type.hashCode +
        (nullableproperty == null ? 0 : nullableproperty.hashCode) +
        otherproperty.hashCode;

  factory ChildWithNullable.fromJson(Map<String, dynamic> json) => _$ChildWithNullableFromJson(json);

  Map<String, dynamic> toJson() => _$ChildWithNullableToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}


enum ChildWithNullableTypeEnum {
@JsonValue(r'ChildWithNullable')
childwithnullable(r'ChildWithNullable'),
@JsonValue(r'unknown_default_open_api')
unknownDefaultOpenApi(r'unknown_default_open_api');

const ChildWithNullableTypeEnum(this.value);

final String value;

@override
String toString() => value;
}


