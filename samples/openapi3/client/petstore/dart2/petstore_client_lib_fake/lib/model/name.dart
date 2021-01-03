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
class Name {
  /// Returns a new [Name] instance.
  Name({
    @required this.name,
    this.snakeCase,
    this.property,
    this.n123number,
  });

  @JsonKey(
    name: r'name',
    required: true,
    
    
  )
  int name;

  @JsonKey(
    name: r'snakeCase',
    
    
    
  )
  int snakeCase;

  @JsonKey(
    name: r'property',
    
    
    
  )
  String property;

  @JsonKey(
    name: r'n123number',
    
    
    
  )
  int n123number;

  @override
  bool operator ==(Object other) => identical(this, other) || other is Name &&
     other.name == name &&
     other.snakeCase == snakeCase &&
     other.property == property &&
     other.n123number == n123number;

  @override
  int get hashCode =>
    (name == null ? 0 : name.hashCode) +
    (snakeCase == null ? 0 : snakeCase.hashCode) +
    (property == null ? 0 : property.hashCode) +
    (n123number == null ? 0 : n123number.hashCode);

  @override
  String toString() => toJson().toString();

  factory Name.fromJson(Map<String, dynamic> json) => _$NameFromJson(json);
  Map<String, dynamic> toJson() => _$NameToJson(this);
}

