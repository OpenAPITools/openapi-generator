//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/one_of_primitive_child.dart';
import 'package:json_annotation/json_annotation.dart';

part 'giga_one_of.g.dart';

@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class GigaOneOf {
  /// Returns a new [GigaOneOf] instance.
  GigaOneOf({
    this.name,
  });

  @JsonKey(name: r'name', required: false, includeIfNull: false)
  final String? name;

  @override
  bool operator ==(Object other) =>
      identical(this, other) || other is GigaOneOf && other.name == name;

  @override
  int get hashCode => name.hashCode;

  factory GigaOneOf.fromJson(Map<String, dynamic> json) =>
      _$GigaOneOfFromJson(json);

  Map<String, dynamic> toJson() => _$GigaOneOfToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }
}
