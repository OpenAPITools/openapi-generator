//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
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
  /// [json] if it's non-null, null if [json] is null.
  static Tag fromJson(Map<String, dynamic> json) => json == null
    ? null
    : Tag(
        id: json[r'id'],
        name: json[r'name'],
    );

  static List<Tag> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <Tag>[]
      : json.map((dynamic value) => Tag.fromJson(value)).toList(growable: true == growable);

  static Map<String, Tag> mapFromJson(Map<String, dynamic> json) {
    final map = <String, Tag>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) => map[key] = Tag.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of Tag-objects as value to a dart map
  static Map<String, List<Tag>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<Tag>>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) {
        map[key] = Tag.listFromJson(value, emptyIsNull: emptyIsNull, growable: growable,);
      });
    }
    return map;
  }
}

