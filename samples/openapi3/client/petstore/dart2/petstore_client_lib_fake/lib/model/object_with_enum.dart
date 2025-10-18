//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.18

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class ObjectWithEnum {
  /// Returns a new [ObjectWithEnum] instance.
  ObjectWithEnum({
    this.attribute = TestEnum.empty,
  });

  TestEnum attribute;

  @override
  bool operator ==(Object other) => identical(this, other) || other is ObjectWithEnum &&
    other.attribute == attribute;

  @override
  int get hashCode =>
    // ignore: unnecessary_parenthesis
    (attribute.hashCode);

  @override
  String toString() => 'ObjectWithEnum[attribute=$attribute]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
      json[r'attribute'] = this.attribute;
    return json;
  }

  /// Returns a new [ObjectWithEnum] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static ObjectWithEnum? fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();

      // Ensure that the map contains the required keys.
      // Note 1: the values aren't checked for validity beyond being non-null.
      // Note 2: this code is stripped in release mode!
      assert(() {
        requiredKeys.forEach((key) {
          assert(json.containsKey(key), 'Required key "ObjectWithEnum[$key]" is missing from JSON.');
          assert(json[key] != null, 'Required key "ObjectWithEnum[$key]" has a null value in JSON.');
        });
        return true;
      }());

      return ObjectWithEnum(
        attribute: TestEnum.fromJson(json[r'attribute']) ?? TestEnum.empty,
      );
    }
    return null;
  }

  static List<ObjectWithEnum> listFromJson(dynamic json, {bool growable = false,}) {
    final result = <ObjectWithEnum>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = ObjectWithEnum.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }

  static Map<String, ObjectWithEnum> mapFromJson(dynamic json) {
    final map = <String, ObjectWithEnum>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = ObjectWithEnum.fromJson(entry.value);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }

  // maps a json object with a list of ObjectWithEnum-objects as value to a dart map
  static Map<String, List<ObjectWithEnum>> mapListFromJson(dynamic json, {bool growable = false,}) {
    final map = <String, List<ObjectWithEnum>>{};
    if (json is Map && json.isNotEmpty) {
      // ignore: parameter_assignments
      json = json.cast<String, dynamic>();
      for (final entry in json.entries) {
        map[entry.key] = ObjectWithEnum.listFromJson(entry.value, growable: growable,);
      }
    }
    return map;
  }

  /// The list of required keys that must be present in a JSON.
  static const requiredKeys = <String>{
  };
}

