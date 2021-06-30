//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
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
  /// [json] if it's non-null, null if [json] is null.
  static Dog fromJson(Map<String, dynamic> json) => json == null
    ? null
    : Dog(
        className: json[r'className'],
        color: json[r'color'],
        breed: json[r'breed'],
    );

  static List<Dog> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <Dog>[]
      : json.map((dynamic value) => Dog.fromJson(value)).toList(growable: true == growable);

  static Map<String, Dog> mapFromJson(Map<String, dynamic> json) {
    final map = <String, Dog>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) => map[key] = Dog.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of Dog-objects as value to a dart map
  static Map<String, List<Dog>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<Dog>>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) {
        map[key] = Dog.listFromJson(value, emptyIsNull: emptyIsNull, growable: growable,);
      });
    }
    return map;
  }
}

