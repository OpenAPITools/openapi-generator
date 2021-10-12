//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.12

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class Cat {
  /// Returns a new [Cat] instance.
  Cat({
    required this.className,
    this.color = 'red',
    this.declawed,
  });


  String className;

  String? color;

  bool? declawed;

  @override
  bool operator ==(Object other) => identical(this, other) || other is Cat &&
     other.className == className &&
     other.color == color &&
     other.declawed == declawed;

  @override
  int get hashCode =>
    className.hashCode +
    color.hashCode +
    declawed.hashCode;

  @override
  String toString() => 'Cat[className=$className, color=$color, declawed=$declawed]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
      json[r'className'] = className;
    if (color != null) {
      json[r'color'] = color;
    }
    if (declawed != null) {
      json[r'declawed'] = declawed;
    }
    return json;
  }

  /// Returns a new [Cat] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static Cat fromJson(Map<String, dynamic> json) => Cat(
        className: json[r'className'] as String,
        color: json[r'color'] as String,
        declawed: json[r'declawed'] as bool,
    );

  static List<Cat> listFromJson(List json, {bool? growable,}) =>
    json.isNotEmpty
      ? json.map<Cat>((i) => Cat.fromJson(i as Map<String, dynamic>)).toList(growable: true == growable)
      : <Cat>[];

  static Map<String, Cat> mapFromJson(dynamic json) {
    final map = <String, Cat>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) => map[key] = Cat.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of Cat-objects as value to a dart map
  static Map<String, List<Cat>> mapListFromJson(dynamic json, {bool? growable,}) {
    final map = <String, List<Cat>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = Cat.listFromJson(
            value,
            growable: growable,
          );
        });
    }
    return map;
  }
}

