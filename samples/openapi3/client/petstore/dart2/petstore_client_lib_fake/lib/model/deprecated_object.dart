//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class DeprecatedObject {
  /// Returns a new [DeprecatedObject] instance.
  DeprecatedObject({
    this.name,
  });

  String name;

  @override
  bool operator ==(Object other) => identical(this, other) || other is DeprecatedObject &&
     other.name == name;

  @override
  int get hashCode =>
    (name == null ? 0 : name.hashCode);

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
  /// [json] if it's non-null, null if [json] is null.
  static DeprecatedObject fromJson(Map<String, dynamic> json) => json == null
    ? null
    : DeprecatedObject(
        name: json[r'name'],
    );

  static List<DeprecatedObject> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <DeprecatedObject>[]
      : json.map((dynamic value) => DeprecatedObject.fromJson(value)).toList(growable: true == growable);

  static Map<String, DeprecatedObject> mapFromJson(Map<String, dynamic> json) {
    final map = <String, DeprecatedObject>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) => map[key] = DeprecatedObject.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of DeprecatedObject-objects as value to a dart map
  static Map<String, List<DeprecatedObject>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<DeprecatedObject>>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) {
        map[key] = DeprecatedObject.listFromJson(value, emptyIsNull: emptyIsNull, growable: growable,);
      });
    }
    return map;
  }
}

