//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class Animal {
  /// Returns a new [Animal] instance.
  Animal({
    @required this.className,
    this.color = 'red',
  });

  String className;

  String color;

  @override
  bool operator ==(Object other) => identical(this, other) || other is Animal &&
     other.className == className &&
     other.color == color;

  @override
  int get hashCode =>
    (className == null ? 0 : className.hashCode) +
    (color == null ? 0 : color.hashCode);

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
  /// [json] if it's non-null, null if [json] is null.
  static Animal fromJson(Map<String, dynamic> json) => json == null
    ? null
    : Animal(
        className: json[r'className'],
        color: json[r'color'],
    );

  static List<Animal> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <Animal>[]
      : json.map((dynamic value) => Animal.fromJson(value)).toList(growable: true == growable);

  static Map<String, Animal> mapFromJson(Map<String, dynamic> json) {
    final map = <String, Animal>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) => map[key] = Animal.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of Animal-objects as value to a dart map
  static Map<String, List<Animal>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<Animal>>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) {
        map[key] = Animal.listFromJson(value, emptyIsNull: emptyIsNull, growable: growable,);
      });
    }
    return map;
  }
}

