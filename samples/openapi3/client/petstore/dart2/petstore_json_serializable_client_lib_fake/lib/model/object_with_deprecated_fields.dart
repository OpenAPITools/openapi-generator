//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.14

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: true,
  explicitToJson: true,
)
class ObjectWithDeprecatedFields {
  /// Returns a new [ObjectWithDeprecatedFields] instance.
  ObjectWithDeprecatedFields({
    this.uuid,
    this.id,
    this.deprecatedRef,
    this.bars = const [],
  });


  @JsonKey(
    name: r'uuid',
    required: false,
  )
  String? uuid;

  @JsonKey(
    name: r'id',
    required: false,
  )
  num? id;

  @JsonKey(
    name: r'deprecatedRef',
    required: false,
  )
  DeprecatedObject? deprecatedRef;

  @JsonKey(
    defaultValue: const [],
    name: r'bars',
    required: false,
  )
  List<String>? bars;

  @override
  bool operator ==(Object other) => identical(this, other) || other is ObjectWithDeprecatedFields &&
     other.uuid == uuid &&
     other.id == id &&
     other.deprecatedRef == deprecatedRef &&
     other.bars == bars;

  @override
  int get hashCode =>
    uuid.hashCode +
    id.hashCode +
    deprecatedRef.hashCode +
    bars.hashCode;

  factory ObjectWithDeprecatedFields.fromJson(Map<String, dynamic> json) => _$ObjectWithDeprecatedFieldsFromJson(json);

  Map<String, dynamic> toJson() => _$ObjectWithDeprecatedFieldsToJson(this);

  @override
  String toString() => toJson().toString();
}

