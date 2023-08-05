//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.12

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class FakeBigDecimalMap200Response {
  /// Returns a new [FakeBigDecimalMap200Response] instance.
  FakeBigDecimalMap200Response({
    this.someId,
    this.someMap = const {},
  });

  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  num? someId;

  Map<String, num> someMap;

  @override
  bool operator ==(Object other) => identical(this, other) || other is FakeBigDecimalMap200Response &&
    other.someId == someId &&
    _deepEquality.equals(other.someMap, someMap);

  @override
  int get hashCode =>
    // ignore: unnecessary_parenthesis
    (someId == null ? 0 : someId!.hashCode) +
    (someMap.hashCode);

  @override
  String toString() => 'FakeBigDecimalMap200Response[someId=$someId, someMap=$someMap]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (this.someId != null) {
      json[r'someId'] = this.someId;
    } else {
      json[r'someId'] = null;
    }
      json[r'someMap'] = this.someMap;
    return json;
  }

  /// Returns a new [FakeBigDecimalMap200Response] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static FakeBigDecimalMap200Response? fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();

      // Ensure that the map contains the required keys.
      // Note 1: the values aren't checked for validity beyond being non-null.
      // Note 2: this code is stripped in release mode!
      assert(() {
        requiredKeys.forEach((key) {
          assert(json.containsKey(key), 'Required key "FakeBigDecimalMap200Response[$key]" is missing from JSON.');
          assert(json[key] != null, 'Required key "FakeBigDecimalMap200Response[$key]" has a null value in JSON.');
        });
        return true;
      }());

      return FakeBigDecimalMap200Response(
        someId: num.parse('${json[r'someId']}'),
        someMap: mapCastOfType<String, num>(json, r'someMap') ?? const {},
      );
    }
    return null;
  }

  static List<FakeBigDecimalMap200Response> listFromJson(dynamic json, {bool growable = false,}) {
    final result = <FakeBigDecimalMap200Response>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = FakeBigDecimalMap200Response.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }

  static Map<String, FakeBigDecimalMap200Response> mapFromJson(dynamic json) {
    final map = <String, FakeBigDecimalMap200Response>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = FakeBigDecimalMap200Response.fromJson(entry.value);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }

  // maps a json object with a list of FakeBigDecimalMap200Response-objects as value to a dart map
  static Map<String, List<FakeBigDecimalMap200Response>> mapListFromJson(dynamic json, {bool growable = false,}) {
    final map = <String, List<FakeBigDecimalMap200Response>>{};
    if (json is Map && json.isNotEmpty) {
      // ignore: parameter_assignments
      json = json.cast<String, dynamic>();
      for (final entry in json.entries) {
        map[entry.key] = FakeBigDecimalMap200Response.listFromJson(entry.value, growable: growable,);
      }
    }
    return map;
  }

  /// The list of required keys that must be present in a JSON.
  static const requiredKeys = <String>{
  };
}

