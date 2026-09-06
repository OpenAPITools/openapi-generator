//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/parent_with_nullable.dart';
import 'package:copy_with_extension/copy_with_extension.dart';
import 'package:json_annotation/json_annotation.dart';

part 'child_with_nullable.g.dart';

// ignore_for_file: unused_import


@CopyWith()
@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class ChildWithNullable extends ParentWithNullable {
/// Returns a new [ChildWithNullable] instance.
  ChildWithNullable({
     this.otherProperty,
     super.type,
     super.nullableProperty,
  });

  @JsonKey(
    
    name: r'otherProperty',
    required: false,
    includeIfNull: false,
  )


  final String? otherProperty;





    @override
    bool operator ==(Object other) => identical(this, other) || other is ChildWithNullable &&
    runtimeType == other.runtimeType &&
      other.type == type &&
      other.nullableProperty == nullableProperty &&
      other.otherProperty == otherProperty;

    @override
    int get hashCode =>
        type.hashCode +
        (nullableProperty == null ? 0 : nullableProperty.hashCode) +
        otherProperty.hashCode;

  factory ChildWithNullable.fromJson(Map<String, dynamic> json) => _$ChildWithNullableFromJson(json);

  Map<String, dynamic> toJson() => _$ChildWithNullableToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

