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
class Animal {
  /// Returns a new [Animal] instance.
  Animal({
    @required this.className,
    this.color = 'red',
  });

  @JsonKey(
    nullable: false,
    name: r'className',
    required: true,
  )
  String className;

  @JsonKey(
    defaultValue: 'red',
    name: r'color',
    required: false,
  )
  String color;

  @override
  bool operator ==(Object other) => identical(this, other) || other is Animal &&
     other.className == className &&
     other.color == color;

  @override
  int get hashCode =>
    (className == null ? 0 : className.hashCode) +
    (color == null ? 0 : color.hashCode);

  factory Animal.fromJson(Map<String, dynamic> json) => _$AnimalFromJson(json);

  Map<String, dynamic> toJson() => _$AnimalToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

