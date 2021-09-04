//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class Dog {
  /// Returns a new [Dog] instance.
  Dog({
    @required this.className,
    this.color = 'red',
    this.breed,
  });

  String className;

  String color;

  String breed;

  @override
  bool operator ==(Object other) => identical(this, other) || other is Dog &&
     other.className == className &&
     other.color == color &&
     other.breed == breed;

  @override
  int get hashCode =>
  // ignore: unnecessary_parenthesis
    (className == null ? 0 : className.hashCode) +
    (color == null ? 0 : color.hashCode) +
    (breed == null ? 0 : breed.hashCode);

  @override
  String toString() => 'Dog[className=$className, color=$color, breed=$breed]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
      json[r'className'] = className;
    if (color != null) {
      json[r'color'] = color;
    }
    if (breed != null) {
      json[r'breed'] = breed;
    }
    return json;
  }

  /// Returns a new [Dog] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static Dog fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();
      return Dog(
        className: mapValueOfType<String>(json, r'className'),
        color: mapValueOfType<String>(json, r'color'),
        breed: mapValueOfType<String>(json, r'breed'),
      );
    }
    return null;
  }

  static List<Dog> listFromJson(dynamic json, {bool emptyIsNull, bool growable,}) =>
    json is List && json.isNotEmpty
      ? json.map(Dog.fromJson).toList(growable: true == growable)
      : true == emptyIsNull ? null : <Dog>[];

  static Map<String, Dog> mapFromJson(dynamic json) {
    final map = <String, Dog>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) => map[key] = Dog.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of Dog-objects as value to a dart map
  static Map<String, List<Dog>> mapListFromJson(dynamic json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<Dog>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = Dog.listFromJson(
            value,
            emptyIsNull: emptyIsNull,
            growable: growable,
          );
        });
    }
    return map;
  }
}

