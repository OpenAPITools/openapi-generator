//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class HasOnlyReadOnly {
  /// Returns a new [HasOnlyReadOnly] instance.
  HasOnlyReadOnly({
    this.bar,
    this.foo,
  });

  String bar;

  String foo;

  @override
  bool operator ==(Object other) => identical(this, other) || other is HasOnlyReadOnly &&
     other.bar == bar &&
     other.foo == foo;

  @override
  int get hashCode =>
  // ignore: unnecessary_parenthesis
    (bar.hashCode) +
    (foo.hashCode);

  @override
  String toString() => 'HasOnlyReadOnly[bar=$bar, foo=$foo]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
      json[r'bar'] = bar;
      json[r'foo'] = foo;
    return json;
  }

  /// The list of required keys that must be present in a JSON.
  static const requiredKeys = <String>{
    
  };

  /// Returns a new [HasOnlyReadOnly] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static HasOnlyReadOnly? fromJson(dynamic value) {
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
              throw FormatException('Required key "HasOnlyReadOnly.$key" is missing from JSON.', json);
            }
            final value = json[key];
            if (null == value) {
              throw FormatException('Required key "HasOnlyReadOnly.$key" cannot be null.', json);
            }
          }
        },
      );

      return HasOnlyReadOnly(
        bar: mapValueOfType<String>(json, r'bar'),
        foo: mapValueOfType<String>(json, r'foo'),
      );
    }
    return null;
  }

  static List<HasOnlyReadOnly>? listFromJson(dynamic json, {bool emptyIsNull = false, bool growable = false,}) {
    final result = <HasOnlyReadOnly>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = HasOnlyReadOnly.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return emptyIsNull ? null : result.toList(growable: growable);
  }

  static Map<String, HasOnlyReadOnly> mapFromJson(dynamic json) {
    final map = <String, HasOnlyReadOnly>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = HasOnlyReadOnly.fromJson(entry.value);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }

  // maps a json object with a list of HasOnlyReadOnly-objects as value to a dart map
  static Map<String, List<HasOnlyReadOnly>> mapListFromJson(dynamic json, {bool emptyIsNull = false, bool growable = false,}) {
    final map = <String, List<HasOnlyReadOnly>>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = HasOnlyReadOnly.listFromJson(entry.value, emptyIsNull: emptyIsNull, growable: growable,);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }
}

