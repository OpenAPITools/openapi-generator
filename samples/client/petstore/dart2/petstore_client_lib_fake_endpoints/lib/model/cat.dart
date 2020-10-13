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

  /// Returns a new [Cat] instance and optionally import its values from
  /// [json] if it's non-null.
  Cat.fromJson(Map<String, dynamic> json) {
    if (json != null) {
      className = json['className'];
      color = json['color'];
      declawed = json['declawed'];
    }
  }

  
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
    className.hashCode +
    color.hashCode +
    declawed.hashCode;

  @override
  String toString() => 'Cat[className=$className, color=$color, declawed=$declawed]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (className != null) {
      json['className'] = className;
    }
    if (color != null) {
      json['color'] = color;
    }
    if (declawed != null) {
      json['declawed'] = declawed;
    }
    return json;
  }

  static List<Cat> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <Cat>[]
      : json.map((v) => Cat.fromJson(v)).toList(growable: true == growable);

  static Map<String, Cat> mapFromJson(Map<String, dynamic> json) {
    final map = <String, Cat>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) => map[key] = Cat.fromJson(v));
    }
    return map;
  }

  // maps a json object with a list of Cat-objects as value to a dart map
  static Map<String, List<Cat>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<Cat>>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) {
        map[key] = Cat.listFromJson(v, emptyIsNull: emptyIsNull, growable: growable);
      });
    }
    return map;
  }
}

