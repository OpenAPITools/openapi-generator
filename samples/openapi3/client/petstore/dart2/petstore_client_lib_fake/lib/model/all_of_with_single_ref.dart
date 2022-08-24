//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.12

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class AllOfWithSingleRef {
  /// Returns a new [AllOfWithSingleRef] instance.
  AllOfWithSingleRef({
    this.username,
    this.singleRefType,
  });

  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  String? username;

  SingleRefType? singleRefType;

  @override
  bool operator ==(Object other) => identical(this, other) || other is AllOfWithSingleRef &&
     other.username == username &&
     other.singleRefType == singleRefType;

  @override
  int get hashCode =>
    // ignore: unnecessary_parenthesis
    (username == null ? 0 : username!.hashCode) +
    (singleRefType == null ? 0 : singleRefType!.hashCode);

  @override
  String toString() => 'AllOfWithSingleRef[username=$username, singleRefType=$singleRefType]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (this.username != null) {
      json[r'username'] = this.username;
    } else {
      json[r'username'] = null;
    }
    if (this.singleRefType != null) {
      json[r'SingleRefType'] = this.singleRefType;
    } else {
      json[r'SingleRefType'] = null;
    }
    return json;
  }

  /// Returns a new [AllOfWithSingleRef] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static AllOfWithSingleRef? fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();

      // Ensure that the map contains the required keys.
      // Note 1: the values aren't checked for validity beyond being non-null.
      // Note 2: this code is stripped in release mode!
      assert(() {
        requiredKeys.forEach((key) {
          assert(json.containsKey(key), 'Required key "AllOfWithSingleRef[$key]" is missing from JSON.');
          assert(json[key] != null, 'Required key "AllOfWithSingleRef[$key]" has a null value in JSON.');
        });
        return true;
      }());

      return AllOfWithSingleRef(
        username: mapValueOfType<String>(json, r'username'),
        singleRefType: SingleRefType.fromJson(json[r'SingleRefType']),
      );
    }
    return null;
  }

  static List<AllOfWithSingleRef>? listFromJson(dynamic json, {bool growable = false,}) {
    final result = <AllOfWithSingleRef>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = AllOfWithSingleRef.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }

  static Map<String, AllOfWithSingleRef> mapFromJson(dynamic json) {
    final map = <String, AllOfWithSingleRef>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = AllOfWithSingleRef.fromJson(entry.value);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }

  // maps a json object with a list of AllOfWithSingleRef-objects as value to a dart map
  static Map<String, List<AllOfWithSingleRef>> mapListFromJson(dynamic json, {bool growable = false,}) {
    final map = <String, List<AllOfWithSingleRef>>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = AllOfWithSingleRef.listFromJson(entry.value, growable: growable,);
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

