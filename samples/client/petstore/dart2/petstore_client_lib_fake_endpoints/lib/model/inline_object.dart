//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class InlineObject {
  /// Returns a new [InlineObject] instance.
  InlineObject({
    this.name,
    this.status,
  });

  /// Returns a new [InlineObject] instance and optionally import its values from
  /// [json] if it's non-null.
  InlineObject.fromJson(Map<String, dynamic> json) {
    if (json != null) {
      name = json['name'];
      status = json['status'];
    }
  }

  /// Updated name of the pet
  String name;

  /// Updated status of the pet
  String status;

  @override
  bool operator ==(Object other) => identical(this, other) || other is InlineObject &&
     other.name == name &&
     other.status == status;

  @override
  int get hashCode =>
    name.hashCode +
    status.hashCode;

  @override
  String toString() => 'InlineObject[name=$name, status=$status]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (name != null) {
      json['name'] = name;
    }
    if (status != null) {
      json['status'] = status;
    }
    return json;
  }

  static List<InlineObject> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <InlineObject>[]
      : json.map((v) => InlineObject.fromJson(v)).toList(growable: true == growable);

  static Map<String, InlineObject> mapFromJson(Map<String, dynamic> json) {
    final map = <String, InlineObject>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) => map[key] = InlineObject.fromJson(v));
    }
    return map;
  }

  // maps a json object with a list of InlineObject-objects as value to a dart map
  static Map<String, List<InlineObject>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<InlineObject>>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) {
        map[key] = InlineObject.listFromJson(v, emptyIsNull: emptyIsNull, growable: growable);
      });
    }
    return map;
  }
}

