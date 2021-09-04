//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class Tag {
  /// Returns a new [Tag] instance.
  Tag({
    this.id,
    this.name,
  });

  int id;

  String name;

  @override
  bool operator ==(Object other) => identical(this, other) || other is Tag &&
     other.id == id &&
     other.name == name;

  @override
  int get hashCode =>
  // ignore: unnecessary_parenthesis
    (id == null ? 0 : id.hashCode) +
    (name == null ? 0 : name.hashCode);

  @override
  String toString() => 'Tag[id=$id, name=$name]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (id != null) {
      json[r'id'] = id;
    }
    if (name != null) {
      json[r'name'] = name;
    }
    return json;
  }

  /// Returns a new [Tag] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static Tag fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();
      return Tag(
        id: mapValueOfType<int>(json, r'id'),
        name: mapValueOfType<String>(json, r'name'),
      );
    }
    return null;
  }

  static List<Tag> listFromJson(dynamic json, {bool emptyIsNull, bool growable,}) =>
    json is List && json.isNotEmpty
      ? json.map(Tag.fromJson).toList(growable: true == growable)
      : true == emptyIsNull ? null : <Tag>[];

  static Map<String, Tag> mapFromJson(dynamic json) {
    final map = <String, Tag>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) => map[key] = Tag.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of Tag-objects as value to a dart map
  static Map<String, List<Tag>> mapListFromJson(dynamic json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<Tag>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = Tag.listFromJson(
            value,
            emptyIsNull: emptyIsNull,
            growable: growable,
          );
        });
    }
    return map;
  }
}

