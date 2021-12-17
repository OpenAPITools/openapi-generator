//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

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

  num myNumber;

  String myString;

  bool myBoolean;

  @override
  bool operator ==(Object other) => identical(this, other) || other is OuterComposite &&
     other.myNumber == myNumber &&
     other.myString == myString &&
     other.myBoolean == myBoolean;

  @override
  int get hashCode =>
  // ignore: unnecessary_parenthesis
    (myNumber.hashCode) +
    (myString.hashCode) +
    (myBoolean.hashCode);

  @override
  String toString() => 'OuterComposite[myNumber=$myNumber, myString=$myString, myBoolean=$myBoolean]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
      json[r'my_number'] = myNumber;
      json[r'my_string'] = myString;
      json[r'my_boolean'] = myBoolean;
    return json;
  }

  /// The list of required keys that must be present in a JSON.
  static const requiredKeys = <String>{
    
  };

  /// Returns a new [OuterComposite] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static OuterComposite? fromJson(dynamic value) {
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
              throw FormatException('Required key "OuterComposite.$key" is missing from JSON.', json);
            }
            final value = json[key];
            if (null == value) {
              throw FormatException('Required key "OuterComposite.$key" cannot be null.', json);
            }
          }
        },
      );

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

  static List<OuterComposite>? listFromJson(dynamic json, {bool emptyIsNull = false, bool growable = false,}) {
    final result = <OuterComposite>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = OuterComposite.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return emptyIsNull ? null : result.toList(growable: growable);
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
  static Map<String, List<OuterComposite>> mapListFromJson(dynamic json, {bool emptyIsNull = false, bool growable = false,}) {
    final map = <String, List<OuterComposite>>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = OuterComposite.listFromJson(entry.value, emptyIsNull: emptyIsNull, growable: growable,);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }
}

