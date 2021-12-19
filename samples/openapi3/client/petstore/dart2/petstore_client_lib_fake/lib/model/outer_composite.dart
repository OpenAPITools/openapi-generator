//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.12

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class OuterComposite {
  /// Returns a new [OuterComposite] instance.
  OuterComposite({
    this.myNumber,
    this.myString,
    this.myBoolean,
  });

  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  num? myNumber;

  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  String? myString;

  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  bool? myBoolean;

  @override
  bool operator ==(Object other) => identical(this, other) || other is OuterComposite &&
     other.myNumber == myNumber &&
     other.myString == myString &&
     other.myBoolean == myBoolean;

  @override
  int get hashCode =>
    // ignore: unnecessary_parenthesis
    (myNumber == null ? 0 : myNumber!.hashCode) +
    (myString == null ? 0 : myString!.hashCode) +
    (myBoolean == null ? 0 : myBoolean!.hashCode);

  @override
  String toString() => 'OuterComposite[myNumber=$myNumber, myString=$myString, myBoolean=$myBoolean]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (myNumber != null) {
      json[r'my_number'] = myNumber;
    }
    if (myString != null) {
      json[r'my_string'] = myString;
    }
    if (myBoolean != null) {
      json[r'my_boolean'] = myBoolean;
    }
    return json;
  }

  /// Returns a new [OuterComposite] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static OuterComposite? fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();

      // Ensure that the map contains the required keys.
      // Note 1: the values aren't checked for validity beyond being non-null.
      // Note 2: this code is stripped in release mode!
      assert(() {
        requiredKeys.forEach((key) {
          assert(json.containsKey(key), 'Required key "OuterComposite[$key]" is missing from JSON.');
          assert(json[key] != null, 'Required key "OuterComposite[$key]" has a null value in JSON.');
        });
        return true;
      }());

      return OuterComposite(
        myNumber: json[r'my_number'] == null
            ? null
            : num.parse(json[r'my_number'].toString()),
        myString: mapValueOfType<String>(json, r'my_string'),
        myBoolean: mapValueOfType<bool>(json, r'my_boolean'),
      );
    }
    return null;
  }

  static List<OuterComposite>? listFromJson(dynamic json, {bool growable = false,}) {
    final result = <OuterComposite>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = OuterComposite.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }

  static Map<String, OuterComposite> mapFromJson(dynamic json) {
    final map = <String, OuterComposite>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = OuterComposite.fromJson(entry.value);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }

  // maps a json object with a list of OuterComposite-objects as value to a dart map
  static Map<String, List<OuterComposite>> mapListFromJson(dynamic json, {bool growable = false,}) {
    final map = <String, List<OuterComposite>>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = OuterComposite.listFromJson(entry.value, growable: growable,);
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

