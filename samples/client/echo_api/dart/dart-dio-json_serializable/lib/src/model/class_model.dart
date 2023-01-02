//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:json_annotation/json_annotation.dart';

part 'class_model.g.dart';

@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class ClassModel {
  /// Returns a new [ClassModel] instance.
  ClassModel({
    this.classField,
  });

  @JsonKey(name: r'_class', required: false, includeIfNull: false)
  final String? classField;

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is ClassModel && other.classField == classField;

  @override
  int get hashCode => classField.hashCode;

  factory ClassModel.fromJson(Map<String, dynamic> json) =>
      _$ClassModelFromJson(json);

  Map<String, dynamic> toJson() => _$ClassModelToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }
}
