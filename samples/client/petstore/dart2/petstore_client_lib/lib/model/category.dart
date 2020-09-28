//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element
// ignore_for_file: always_put_required_named_parameters_first

part of openapi.api;

class Category {
  Category({
    this.id,
    this.name,
  });

  
  int id;

  
  String name;

  @override
  String toString() => 'Category[id=$id, name=$name, ]';

  Category.fromJson(Map<String, dynamic> json) {
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

  static List<Category> listFromJson(List<dynamic> json, {bool growable}) =>
    json == null
      ? <Category>[]
      : json.map((v) => Category.fromJson(v)).toList(growable: true == growable);

  static Map<String, Category> mapFromJson(Map<String, dynamic> json) {
    final map = <String, Category>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) => map[key] = Category.fromJson(v));
    }
    return map;
  }

  // maps a json object with a list of Category-objects as value to a dart map
  static Map<String, List<Category>> mapListFromJson(Map<String, dynamic> json, {bool growable}) {
    final map = <String, List<Category>>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) {
        map[key] = Category.listFromJson(v, growable: growable);
      });
    }
    return map;
  }
}

