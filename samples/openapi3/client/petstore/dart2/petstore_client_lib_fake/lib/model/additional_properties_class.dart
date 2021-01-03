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
  includeIfNull: false,
  disallowUnrecognizedKeys: true,
)
class AdditionalPropertiesClass {
  /// Returns a new [AdditionalPropertiesClass] instance.
  AdditionalPropertiesClass({
    this.mapProperty = const {},
    this.mapOfMapProperty = const {},
  });

  @JsonKey(
    name: r'mapProperty',
    
    defaultValue: const {},
    
  )
  Map<String, String> mapProperty;

  @JsonKey(
    name: r'mapOfMapProperty',
    
    defaultValue: const {},
    
  )
  Map<String, Map<String, String>> mapOfMapProperty;

  @override
  bool operator ==(Object other) => identical(this, other) || other is AdditionalPropertiesClass &&
     other.mapProperty == mapProperty &&
     other.mapOfMapProperty == mapOfMapProperty;

  @override
  int get hashCode =>
    (mapProperty == null ? 0 : mapProperty.hashCode) +
    (mapOfMapProperty == null ? 0 : mapOfMapProperty.hashCode);

  @override
  String toString() => toJson().toString();

  factory AdditionalPropertiesClass.fromJson(Map<String, dynamic> json) => _$AdditionalPropertiesClassFromJson(json);
  Map<String, dynamic> toJson() => _$AdditionalPropertiesClassToJson(this);
}

