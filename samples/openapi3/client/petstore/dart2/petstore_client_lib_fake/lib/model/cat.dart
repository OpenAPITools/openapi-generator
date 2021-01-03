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
class Cat {
  /// Returns a new [Cat] instance.
  Cat({
    @required this.className,
    this.color = 'red',
    this.declawed,
  });

  @JsonKey(
    name: r'className',
    required: true,
    
    
  )
  String className;

  @JsonKey(
    name: r'color',
    
    defaultValue: 'red',
    
  )
  String color;

  @JsonKey(
    name: r'declawed',
    
    
    
  )
  bool declawed;

  @override
  bool operator ==(Object other) => identical(this, other) || other is Cat &&
     other.className == className &&
     other.color == color &&
     other.declawed == declawed;

  @override
  int get hashCode =>
    (className == null ? 0 : className.hashCode) +
    (color == null ? 0 : color.hashCode) +
    (declawed == null ? 0 : declawed.hashCode);

  @override
  String toString() => toJson().toString();

  factory Cat.fromJson(Map<String, dynamic> json) => _$CatFromJson(json);
  Map<String, dynamic> toJson() => _$CatToJson(this);
}

