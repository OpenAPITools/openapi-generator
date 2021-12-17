//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class Category {
  /// Returns a new [Category] instance.
  Category({
    this.id,
    required this.name,
  });

  int id;

  String name;

  @override
  bool operator ==(Object other) => identical(this, other) || other is Category &&
     other.id == id &&
     other.name == name;

  @override
  int get hashCode =>
  // ignore: unnecessary_parenthesis
    (id.hashCode) +
    (name.hashCode);

  @override
  String toString() => 'Category[id=$id, name=$name]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
      json[r'id'] = id;
      json[r'name'] = name;
    return json;
  }

  /// The list of required keys that must be present in a JSON.
  static const requiredKeys = <String>{
    'name',

  };

  /// Returns a new [Category] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static Category? fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();

      // Ensure that the map contains the required keys.
      // Note 1: the values aren't checked for validity beyond being non-null.
      // Note 2: this code is stripped in release mode!
      assert(
        false,
        () {
          for (final key in requiredKeys) {
            if (!json.containsKey(key)) {
              throw FormatException('Required key "Category.$key" is missing from JSON.', json);
            }
            final value = json[key];
            if (null == value) {
              throw FormatException('Required key "Category.$key" cannot be null.', json);
            }
          }
        },
      );

      return Category(
        id: mapValueOfType<int>(json, r'id'),
        name: mapValueOfType<String>(json, r'name')!,
      );
    }
    return null;
  }

  static List<Category>? listFromJson(dynamic json, {bool emptyIsNull = false, bool growable = false,}) {
    final result = <Category>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = Category.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return emptyIsNull ? null : result.toList(growable: growable);
  }

  static Map<String, Category> mapFromJson(dynamic json) {
    final map = <String, Category>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = Category.fromJson(entry.value);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }

  // maps a json object with a list of Category-objects as value to a dart map
  static Map<String, List<Category>> mapListFromJson(dynamic json, {bool emptyIsNull = false, bool growable = false,}) {
    final map = <String, List<Category>>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = Category.listFromJson(entry.value, emptyIsNull: emptyIsNull, growable: growable,);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }
}

