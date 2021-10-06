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
class MixedPropertiesAndAdditionalPropertiesClass {
  /// Returns a new [MixedPropertiesAndAdditionalPropertiesClass] instance.
  MixedPropertiesAndAdditionalPropertiesClass({
    this.uuid,
    this.dateTime,
    this.map = const {},
  });


  @JsonKey(
    name: r'uuid',
    required: false,
  )
  String? uuid;

  @JsonKey(
    name: r'dateTime',
    required: false,
  )
  DateTime? dateTime;

  @JsonKey(
    defaultValue: const {},
    name: r'map',
    required: false,
  )
  Map<String, Animal>? map;

  @override
  bool operator ==(Object other) => identical(this, other) || other is MixedPropertiesAndAdditionalPropertiesClass &&
     other.uuid == uuid &&
     other.dateTime == dateTime &&
     other.map == map;

  @override
  int get hashCode =>
    uuid.hashCode +
    dateTime.hashCode +
    map.hashCode;

  factory MixedPropertiesAndAdditionalPropertiesClass.fromJson(Map<String, dynamic> json) => _$MixedPropertiesAndAdditionalPropertiesClassFromJson(json);

  Map<String, dynamic> toJson() => _$MixedPropertiesAndAdditionalPropertiesClassToJson(this);

  @override
  String toString() => toJson().toString();
}

