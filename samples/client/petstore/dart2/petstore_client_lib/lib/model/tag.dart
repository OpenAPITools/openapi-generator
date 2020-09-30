//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: avoid_init_to_null, lines_longer_than_80_chars
// ignore_for_file: prefer_single_quotes

part of openapi.api;

/// [String] values for all properties defined in [Tag].
abstract class TagStrings {
  const TagStrings._();

  static const id_ = "id";
  static const name_ = "name";
}

class Tag {
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
    id.hashCode +
    name.hashCode;

  @override
  String toString() => _toString("");

  Tag.fromJson(Map<String, dynamic> json) {
    if (json == null) {
      return;
    }
    id = json[TagStrings.id_];
    name = json[TagStrings.name_];
  }

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (id != null) {
      json[TagStrings.id_] = id;
    }
    if (name != null) {
      json[TagStrings.name_] = name;
    }
    return json;
  }

  String _toString(String prefix) {
    final sb = StringBuffer();

    sb.write("Tag=[");

    sb.write("\n$prefix  ");
    sb.write(TagStrings.id_);
    sb.write(": ");
    sb.write(id);
  sb.write(",");

    sb.write("\n$prefix  ");
    sb.write(TagStrings.name_);
    sb.write(": ");
    sb.write(name);
  

    sb.write("\n$prefix]");

    return sb.toString();
  }

  static List<Tag> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <Tag>[]
      : json.map((v) => Tag.fromJson(v)).toList(growable: true == growable);

  static Map<String, Tag> mapFromJson(Map<String, dynamic> json) {
    final map = <String, Tag>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) => map[key] = Tag.fromJson(v));
    }
    return map;
  }

  // maps a json object with a list of Tag-objects as value to a dart map
  static Map<String, List<Tag>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable}) {
    final map = <String, List<Tag>>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) {
        map[key] = Tag.listFromJson(v, emptyIsNull: emptyIsNull, growable: growable);
      });
    }
    return map;
  }
}

