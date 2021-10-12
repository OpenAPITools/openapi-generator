//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.12

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class Animal {
  /// Returns a new [Animal] instance.
  Animal({
    required this.className,
    this.color = 'red',
  });


  String className;

  String? color;

  @override
  bool operator ==(Object other) => identical(this, other) || other is Animal &&
     other.className == className &&
     other.color == color;

  @override
  int get hashCode =>
    className.hashCode +
    color.hashCode;

  @override
  String toString() => 'Animal[className=$className, color=$color]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
      json[r'className'] = className;
    if (color != null) {
      json[r'color'] = color;
    }
    return json;
  }

  /// Returns a new [Animal] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static Animal fromJson(Map<String, dynamic> json) => Animal(
        className: json[r'className'] as String,
        color: json[r'color'] as String,
    );

  static List<Animal> listFromJson(List json, {bool? growable,}) =>
    json.isNotEmpty
      ? json.map<Animal>((i) => Animal.fromJson(i as Map<String, dynamic>)).toList(growable: true == growable)
      : <Animal>[];

  static Map<String, Animal> mapFromJson(dynamic json) {
    final map = <String, Animal>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) => map[key] = Animal.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of Animal-objects as value to a dart map
  static Map<String, List<Animal>> mapListFromJson(dynamic json, {bool? growable,}) {
    final map = <String, List<Animal>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = Animal.listFromJson(
            value,
            growable: growable,
          );
        });
    }
    return map;
  }
}

