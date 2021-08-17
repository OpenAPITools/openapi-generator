//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: true,
  explicitToJson: true,
)
class ClassModel {
  /// Returns a new [ClassModel] instance.
  ClassModel({
    this.class_,
  });

  @JsonKey(
    nullable: false,
    name: r'_class',
    required: false,
  )
  String class_;

  @override
  bool operator ==(Object other) => identical(this, other) || other is ClassModel &&
     other.class_ == class_;

  @override
  int get hashCode =>
    (class_ == null ? 0 : class_.hashCode);

  factory ClassModel.fromJson(Map<String, dynamic> json) => _$ClassModelFromJson(json);

  Map<String, dynamic> toJson() => _$ClassModelToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

