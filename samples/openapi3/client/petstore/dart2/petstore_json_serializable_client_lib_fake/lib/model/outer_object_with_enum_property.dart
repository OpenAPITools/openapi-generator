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
class OuterObjectWithEnumProperty {
  /// Returns a new [OuterObjectWithEnumProperty] instance.
  OuterObjectWithEnumProperty({
    @required this.value,
  });

  @JsonKey(
    nullable: false,
    name: r'value',
    required: true,
  )
  OuterEnumInteger value;

  @override
  bool operator ==(Object other) => identical(this, other) || other is OuterObjectWithEnumProperty &&
     other.value == value;

  @override
  int get hashCode =>
    (value == null ? 0 : value.hashCode);

  factory OuterObjectWithEnumProperty.fromJson(Map<String, dynamic> json) => _$OuterObjectWithEnumPropertyFromJson(json);

  Map<String, dynamic> toJson() => _$OuterObjectWithEnumPropertyToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

