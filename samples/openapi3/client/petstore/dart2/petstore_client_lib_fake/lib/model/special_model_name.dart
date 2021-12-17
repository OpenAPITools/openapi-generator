//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class SpecialModelName {
  /// Returns a new [SpecialModelName] instance.
  SpecialModelName({
    this.dollarSpecialLeftSquareBracketPropertyPeriodNameRightSquareBracket,
  });

  int dollarSpecialLeftSquareBracketPropertyPeriodNameRightSquareBracket;

  @override
  bool operator ==(Object other) => identical(this, other) || other is SpecialModelName &&
     other.dollarSpecialLeftSquareBracketPropertyPeriodNameRightSquareBracket == dollarSpecialLeftSquareBracketPropertyPeriodNameRightSquareBracket;

  @override
  int get hashCode =>
  // ignore: unnecessary_parenthesis
    (dollarSpecialLeftSquareBracketPropertyPeriodNameRightSquareBracket.hashCode);

  @override
  String toString() => 'SpecialModelName[dollarSpecialLeftSquareBracketPropertyPeriodNameRightSquareBracket=$dollarSpecialLeftSquareBracketPropertyPeriodNameRightSquareBracket]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
      json[r'$special[property.name]'] = dollarSpecialLeftSquareBracketPropertyPeriodNameRightSquareBracket;
    return json;
  }

  /// The list of required keys that must be present in a JSON.
  static const requiredKeys = <String>{
    
  };

  /// Returns a new [SpecialModelName] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static SpecialModelName? fromJson(dynamic value) {
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
              throw FormatException('Required key "SpecialModelName.$key" is missing from JSON.', json);
            }
            final value = json[key];
            if (null == value) {
              throw FormatException('Required key "SpecialModelName.$key" cannot be null.', json);
            }
          }
        },
      );

      return SpecialModelName(
        dollarSpecialLeftSquareBracketPropertyPeriodNameRightSquareBracket: mapValueOfType<int>(json, r'$special[property.name]'),
      );
    }
    return null;
  }

  static List<SpecialModelName>? listFromJson(dynamic json, {bool emptyIsNull = false, bool growable = false,}) {
    final result = <SpecialModelName>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = SpecialModelName.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return emptyIsNull ? null : result.toList(growable: growable);
  }

  static Map<String, SpecialModelName> mapFromJson(dynamic json) {
    final map = <String, SpecialModelName>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = SpecialModelName.fromJson(entry.value);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }

  // maps a json object with a list of SpecialModelName-objects as value to a dart map
  static Map<String, List<SpecialModelName>> mapListFromJson(dynamic json, {bool emptyIsNull = false, bool growable = false,}) {
    final map = <String, List<SpecialModelName>>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = SpecialModelName.listFromJson(entry.value, emptyIsNull: emptyIsNull, growable: growable,);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }
}

