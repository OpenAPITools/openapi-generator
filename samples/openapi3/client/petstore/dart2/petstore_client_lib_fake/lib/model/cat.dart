//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class Cat {
  /// Returns a new [Cat] instance.
  Cat({
    @required this.className,
    this.color = 'red',
    this.declawed,
  });

  String className;

  String color;

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
  /// [json] if it's non-null, null if [json] is null.
  static Cat fromJson(Map<String, dynamic> json) => json == null
    ? null
    : Cat(
        className: json[r'className'],
        color: json[r'color'],
        declawed: json[r'declawed'],
    );

  static List<Cat> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <Cat>[]
      : json.map((dynamic value) => Cat.fromJson(value)).toList(growable: true == growable);

  static Map<String, Cat> mapFromJson(Map<String, dynamic> json) {
    final map = <String, Cat>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) => map[key] = Cat.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of Cat-objects as value to a dart map
  static Map<String, List<Cat>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<Cat>>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) {
        map[key] = Cat.listFromJson(value, emptyIsNull: emptyIsNull, growable: growable,);
      });
    }
    return map;
  }
}

