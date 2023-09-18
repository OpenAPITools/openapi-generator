//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.12

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class TestInlineFreeformAdditionalPropertiesRequest {
  /// Returns a new [TestInlineFreeformAdditionalPropertiesRequest] instance.
  TestInlineFreeformAdditionalPropertiesRequest({
    this.someProperty,
  });

  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  String? someProperty;

  @override
  bool operator ==(Object other) => identical(this, other) || other is TestInlineFreeformAdditionalPropertiesRequest &&
    other.someProperty == someProperty;

  @override
  int get hashCode =>
    // ignore: unnecessary_parenthesis
    (someProperty == null ? 0 : someProperty!.hashCode);

  @override
  String toString() => 'TestInlineFreeformAdditionalPropertiesRequest[someProperty=$someProperty]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (this.someProperty != null) {
      json[r'someProperty'] = this.someProperty;
    } else {
      json[r'someProperty'] = null;
    }
    return json;
  }

  /// Returns a new [TestInlineFreeformAdditionalPropertiesRequest] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static TestInlineFreeformAdditionalPropertiesRequest? fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();

      // Ensure that the map contains the required keys.
      // Note 1: the values aren't checked for validity beyond being non-null.
      // Note 2: this code is stripped in release mode!
      assert(() {
        requiredKeys.forEach((key) {
          assert(json.containsKey(key), 'Required key "TestInlineFreeformAdditionalPropertiesRequest[$key]" is missing from JSON.');
          assert(json[key] != null, 'Required key "TestInlineFreeformAdditionalPropertiesRequest[$key]" has a null value in JSON.');
        });
        return true;
      }());

      return TestInlineFreeformAdditionalPropertiesRequest(
        someProperty: mapValueOfType<String>(json, r'someProperty'),
      );
    }
    return null;
  }

  static List<TestInlineFreeformAdditionalPropertiesRequest> listFromJson(dynamic json, {bool growable = false,}) {
    final result = <TestInlineFreeformAdditionalPropertiesRequest>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = TestInlineFreeformAdditionalPropertiesRequest.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }

  static Map<String, TestInlineFreeformAdditionalPropertiesRequest> mapFromJson(dynamic json) {
    final map = <String, TestInlineFreeformAdditionalPropertiesRequest>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = TestInlineFreeformAdditionalPropertiesRequest.fromJson(entry.value);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }

  // maps a json object with a list of TestInlineFreeformAdditionalPropertiesRequest-objects as value to a dart map
  static Map<String, List<TestInlineFreeformAdditionalPropertiesRequest>> mapListFromJson(dynamic json, {bool growable = false,}) {
    final map = <String, List<TestInlineFreeformAdditionalPropertiesRequest>>{};
    if (json is Map && json.isNotEmpty) {
      // ignore: parameter_assignments
      json = json.cast<String, dynamic>();
      for (final entry in json.entries) {
        map[entry.key] = TestInlineFreeformAdditionalPropertiesRequest.listFromJson(entry.value, growable: growable,);
      }
    }
    return map;
  }

  /// The list of required keys that must be present in a JSON.
  static const requiredKeys = <String>{
  };
}

