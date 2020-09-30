//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: avoid_init_to_null, lines_longer_than_80_chars
// ignore_for_file: prefer_single_quotes

part of openapi.api;

/// [String] values for all properties defined in [Category].
abstract class CategoryStrings {
  const CategoryStrings._();

  static const id_ = "id";
  static const name_ = "name";
}

class Category {
  Category({
    this.id,
    this.name,
  });

  
  int id;

  
  String name;

  @override
  bool operator ==(Object other) => identical(this, other) || other is Category &&
     other.id == id &&
     other.name == name;

  @override
  int get hashCode =>
    id.hashCode +
    name.hashCode;

  @override
  String toString() => _toString("");

  Category.fromJson(Map<String, dynamic> json) {
    if (json == null) {
      return;
    }
    id = json[CategoryStrings.id_];
    name = json[CategoryStrings.name_];
  }

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (id != null) {
      json[CategoryStrings.id_] = id;
    }
    if (name != null) {
      json[CategoryStrings.name_] = name;
    }
    return json;
  }

  String _toString(String prefix) {
    final sb = StringBuffer();

    sb.write("Category=[");

    sb.write("\n$prefix  ");
    sb.write(CategoryStrings.id_);
    sb.write(": ");
    sb.write(id);
  sb.write(",");

    sb.write("\n$prefix  ");
    sb.write(CategoryStrings.name_);
    sb.write(": ");
    sb.write(name);
  

    sb.write("\n$prefix]");

    return sb.toString();
  }

  static List<Category> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <Category>[]
      : json.map((v) => Category.fromJson(v)).toList(growable: true == growable);

  static Map<String, Category> mapFromJson(Map<String, dynamic> json) {
    final map = <String, Category>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) => map[key] = Category.fromJson(v));
    }
    return map;
  }

  // maps a json object with a list of Category-objects as value to a dart map
  static Map<String, List<Category>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable}) {
    final map = <String, List<Category>>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) {
        map[key] = Category.listFromJson(v, emptyIsNull: emptyIsNull, growable: growable);
      });
    }
    return map;
  }
}

