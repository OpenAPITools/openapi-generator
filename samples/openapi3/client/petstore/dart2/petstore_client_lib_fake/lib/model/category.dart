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
class Category {
  /// Returns a new [Category] instance.
  Category({
    this.id,
    this.name = 'default-name',
  });

  @JsonKey(
    name: r'id',
    
    
    
  )
  int id;

  @JsonKey(
    name: r'name',
    required: true,
    defaultValue: 'default-name',
    
  )
  String name;

  @override
  bool operator ==(Object other) => identical(this, other) || other is Category &&
     other.id == id &&
     other.name == name;

  @override
  int get hashCode =>
    (id == null ? 0 : id.hashCode) +
    (name == null ? 0 : name.hashCode);

  @override
  String toString() => toJson().toString();

  factory Category.fromJson(Map<String, dynamic> json) => _$CategoryFromJson(json);
  Map<String, dynamic> toJson() => _$CategoryToJson(this);
}

