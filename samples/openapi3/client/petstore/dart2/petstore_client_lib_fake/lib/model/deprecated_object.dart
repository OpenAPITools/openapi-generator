//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.14

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class DeprecatedObject {
  /// Returns a new [DeprecatedObject] instance.
  DeprecatedObject({
    this.name,
  });


  String? name;

  @override
  bool operator ==(Object other) => identical(this, other) || other is DeprecatedObject &&
     other.name == name;

  @override
  int get hashCode =>
    name.hashCode;

  @override
  String toString() => 'DeprecatedObject[name=$name]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (name != null) {
      json[r'name'] = name;
    }
    return json;
  }

  /// Returns a new [DeprecatedObject] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static DeprecatedObject fromJson(Map<String, dynamic> json) => DeprecatedObject(
        name: json[r'name'] as String,
    );

  static List<DeprecatedObject> listFromJson(List json, {bool? growable,}) =>
    json.isNotEmpty
      ? json.map<DeprecatedObject>((i) => DeprecatedObject.fromJson(i as Map<String, dynamic>)).toList(growable: true == growable)
      : <DeprecatedObject>[];

  static Map<String, DeprecatedObject> mapFromJson(dynamic json) {
    final map = <String, DeprecatedObject>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) => map[key] = DeprecatedObject.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of DeprecatedObject-objects as value to a dart map
  static Map<String, List<DeprecatedObject>> mapListFromJson(dynamic json, {bool? growable,}) {
    final map = <String, List<DeprecatedObject>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = DeprecatedObject.listFromJson(
            value,
            growable: growable,
          );
        });
    }
    return map;
  }
}

