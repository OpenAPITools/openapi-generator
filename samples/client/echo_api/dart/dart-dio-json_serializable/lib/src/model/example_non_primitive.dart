//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';

part 'example_non_primitive.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class ExampleNonPrimitive {
  /// Returns a new [ExampleNonPrimitive] instance.
  ExampleNonPrimitive({
  });

  @override
  bool operator ==(Object other) => identical(this, other) || other is ExampleNonPrimitive &&

  @override
  int get hashCode =>

  factory ExampleNonPrimitive.fromJson(Map<String, dynamic> json) => _$ExampleNonPrimitiveFromJson(json);

  Map<String, dynamic> toJson() => _$ExampleNonPrimitiveToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

