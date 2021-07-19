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
class DeprecatedObject {
  /// Returns a new [DeprecatedObject] instance.
  DeprecatedObject({
    this.name,
  });

  @JsonKey(
    nullable: false,
    name: r'name',
    required: false,
  )
  String name;

  @override
  bool operator ==(Object other) => identical(this, other) || other is DeprecatedObject &&
     other.name == name;

  @override
  int get hashCode =>
    (name == null ? 0 : name.hashCode);

  factory DeprecatedObject.fromJson(Map<String, dynamic> json) => _$DeprecatedObjectFromJson(json);

  Map<String, dynamic> toJson() => _$DeprecatedObjectToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

