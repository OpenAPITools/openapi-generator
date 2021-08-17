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
class Dog {
  /// Returns a new [Dog] instance.
  Dog({
    @required this.className,
    this.color = 'red',
    this.breed,
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

  @JsonKey(
    nullable: false,
    name: r'breed',
    required: false,
  )
  String breed;

  @override
  bool operator ==(Object other) => identical(this, other) || other is Dog &&
     other.className == className &&
     other.color == color &&
     other.breed == breed;

  @override
  int get hashCode =>
    (className == null ? 0 : className.hashCode) +
    (color == null ? 0 : color.hashCode) +
    (breed == null ? 0 : breed.hashCode);

  factory Dog.fromJson(Map<String, dynamic> json) => _$DogFromJson(json);

  Map<String, dynamic> toJson() => _$DogToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

