//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.12

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class Dog {
  /// Returns a new [Dog] instance.
  Dog({
    required this.className,
    this.color = 'red',
    this.breed,
  });


  String className;

  String? color;

  String? breed;

  @override
  bool operator ==(Object other) => identical(this, other) || other is Dog &&
     other.className == className &&
     other.color == color &&
     other.breed == breed;

  @override
  int get hashCode =>
    className.hashCode +
    color.hashCode +
    breed.hashCode;

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
  static Dog fromJson(Map<String, dynamic> json) => Dog(
        className: json[r'className'] as String,
        color: json[r'color'] as String,
        breed: json[r'breed'] as String,
    );

  static List<Dog> listFromJson(List json, {bool? growable,}) =>
    json.isNotEmpty
      ? json.map<Dog>((i) => Dog.fromJson(i as Map<String, dynamic>)).toList(growable: true == growable)
      : <Dog>[];

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
  static Map<String, List<Dog>> mapListFromJson(dynamic json, {bool? growable,}) {
    final map = <String, List<Dog>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = Dog.listFromJson(
            value,
            growable: growable,
          );
        });
    }
    return map;
  }
}

