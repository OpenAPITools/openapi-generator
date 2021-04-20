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
class Cat {
  /// Returns a new [Cat] instance.
  Cat({
    @required this.className,
    this.color = 'red',
    this.declawed,
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
    name: r'declawed',
    required: false,
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

  factory Cat.fromJson(Map<String, dynamic> json) => _$CatFromJson(json);

  Map<String, dynamic> toJson() => _$CatToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

