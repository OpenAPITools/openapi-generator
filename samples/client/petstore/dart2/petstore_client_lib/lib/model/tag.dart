//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element
// ignore_for_file: always_put_required_named_parameters_first

part of openapi.api;

class Tag {
  Tag({
    this.id,
    this.name,
  });

  
  int id;

  
  String name;

  @override
  String toString() => 'Tag[id=$id, name=$name, ]';

  Tag.fromJson(Map<String, dynamic> json) {
    if (json == null) return;
    id = json['id'];
    name = json['name'];
  }

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (id != null)
      json['id'] = id;
    if (name != null)
      json['name'] = name;
    return json;
  }

  static List<Tag> listFromJson(List<dynamic> json, {bool growable}) =>
    json == null
      ? <Tag>[]
      : json.map((v) => Tag.fromJson(v)).toList(growable: true == growable);

  static Map<String, Tag> mapFromJson(Map<String, dynamic> json) {
    final map = <String, Tag>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) => map[key] = Tag.fromJson(v));
    }
    return map;
  }

  // maps a json object with a list of Tag-objects as value to a dart map
  static Map<String, List<Tag>> mapListFromJson(Map<String, dynamic> json, {bool growable}) {
    final map = <String, List<Tag>>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) {
        map[key] = Tag.listFromJson(v, growable: growable);
      });
    }
    return map;
  }
}

