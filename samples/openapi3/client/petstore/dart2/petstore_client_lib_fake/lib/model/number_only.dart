//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.12

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class NumberOnly {
  /// Returns a new [NumberOnly] instance.
  NumberOnly({
    this.justNumber,
  });

  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  num? justNumber;

  @override
  bool operator ==(Object other) => identical(this, other) || other is NumberOnly &&
     other.justNumber == justNumber;

  @override
  int get hashCode =>
    // ignore: unnecessary_parenthesis
    (justNumber == null ? 0 : justNumber!.hashCode);

  @override
  String toString() => 'NumberOnly[justNumber=$justNumber]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (this.justNumber != null) {
      json[r'JustNumber'] = this.justNumber;
    } else {
      json[r'JustNumber'] = null;
    }
    return json;
  }

  /// Returns a new [NumberOnly] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static NumberOnly? fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();

      // Ensure that the map contains the required keys.
      // Note 1: the values aren't checked for validity beyond being non-null.
      // Note 2: this code is stripped in release mode!
      assert(() {
        requiredKeys.forEach((key) {
          assert(json.containsKey(key), 'Required key "NumberOnly[$key]" is missing from JSON.');
          assert(json[key] != null, 'Required key "NumberOnly[$key]" has a null value in JSON.');
        });
        return true;
      }());

      return NumberOnly(
        justNumber: json[r'JustNumber'] == null
            ? null
            : num.parse(json[r'JustNumber'].toString()),
      );
    }
    return null;
  }

  static List<NumberOnly>? listFromJson(dynamic json, {bool growable = false,}) {
    final result = <NumberOnly>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = NumberOnly.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }

  static Map<String, NumberOnly> mapFromJson(dynamic json) {
    final map = <String, NumberOnly>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = NumberOnly.fromJson(entry.value);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }

  // maps a json object with a list of NumberOnly-objects as value to a dart map
  static Map<String, List<NumberOnly>> mapListFromJson(dynamic json, {bool growable = false,}) {
    final map = <String, List<NumberOnly>>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = NumberOnly.listFromJson(entry.value, growable: growable,);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }

  /// The list of required keys that must be present in a JSON.
  static const requiredKeys = <String>{
  };
}

