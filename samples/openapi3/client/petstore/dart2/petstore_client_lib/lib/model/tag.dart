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
class Tag {
  /// Returns a new [Tag] instance.
  Tag({
    this.id,
    this.name,
  });

  @JsonKey(
    name: r'id',
    
    
    
  )
  int id;

  @JsonKey(
    name: r'name',
    
    
    
  )
  String name;

  @override
  bool operator ==(Object other) => identical(this, other) || other is Tag &&
     other.id == id &&
     other.name == name;

  @override
  int get hashCode =>
    (id == null ? 0 : id.hashCode) +
    (name == null ? 0 : name.hashCode);

  @override
  String toString() => toJson().toString();

  factory Tag.fromJson(Map<String, dynamic> json) => _$TagFromJson(json);
  Map<String, dynamic> toJson() => _$TagToJson(this);
}

