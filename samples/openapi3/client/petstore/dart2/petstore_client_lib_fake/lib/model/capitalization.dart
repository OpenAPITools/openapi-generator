//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class Capitalization {
  /// Returns a new [Capitalization] instance.
  Capitalization({
    this.smallCamel,
    this.capitalCamel,
    this.smallSnake,
    this.capitalSnake,
    this.sCAETHFlowPoints,
    this.ATT_NAME,
  });

  String smallCamel;

  String capitalCamel;

  String smallSnake;

  String capitalSnake;

  String sCAETHFlowPoints;

  /// Name of the pet 
  String ATT_NAME;

  @override
  bool operator ==(Object other) => identical(this, other) || other is Capitalization &&
     other.smallCamel == smallCamel &&
     other.capitalCamel == capitalCamel &&
     other.smallSnake == smallSnake &&
     other.capitalSnake == capitalSnake &&
     other.sCAETHFlowPoints == sCAETHFlowPoints &&
     other.ATT_NAME == ATT_NAME;

  @override
  int get hashCode =>
  // ignore: unnecessary_parenthesis
    (smallCamel.hashCode) +
    (capitalCamel.hashCode) +
    (smallSnake.hashCode) +
    (capitalSnake.hashCode) +
    (sCAETHFlowPoints.hashCode) +
    (ATT_NAME.hashCode);

  @override
  String toString() => 'Capitalization[smallCamel=$smallCamel, capitalCamel=$capitalCamel, smallSnake=$smallSnake, capitalSnake=$capitalSnake, sCAETHFlowPoints=$sCAETHFlowPoints, ATT_NAME=$ATT_NAME]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
      json[r'smallCamel'] = smallCamel;
      json[r'CapitalCamel'] = capitalCamel;
      json[r'small_Snake'] = smallSnake;
      json[r'Capital_Snake'] = capitalSnake;
      json[r'SCA_ETH_Flow_Points'] = sCAETHFlowPoints;
      json[r'ATT_NAME'] = ATT_NAME;
    return json;
  }

  /// The list of required keys that must be present in a JSON.
  static const requiredKeys = <String>{
    
  };

  /// Returns a new [Capitalization] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static Capitalization? fromJson(dynamic value) {
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
              throw FormatException('Required key "Capitalization.$key" is missing from JSON.', json);
            }
            final value = json[key];
            if (null == value) {
              throw FormatException('Required key "Capitalization.$key" cannot be null.', json);
            }
          }
        },
      );

      return Capitalization(
        smallCamel: mapValueOfType<String>(json, r'smallCamel'),
        capitalCamel: mapValueOfType<String>(json, r'CapitalCamel'),
        smallSnake: mapValueOfType<String>(json, r'small_Snake'),
        capitalSnake: mapValueOfType<String>(json, r'Capital_Snake'),
        sCAETHFlowPoints: mapValueOfType<String>(json, r'SCA_ETH_Flow_Points'),
        ATT_NAME: mapValueOfType<String>(json, r'ATT_NAME'),
      );
    }
    return null;
  }

  static List<Capitalization>? listFromJson(dynamic json, {bool emptyIsNull = false, bool growable = false,}) {
    final result = <Capitalization>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = Capitalization.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return emptyIsNull ? null : result.toList(growable: growable);
  }

  static Map<String, Capitalization> mapFromJson(dynamic json) {
    final map = <String, Capitalization>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = Capitalization.fromJson(entry.value);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }

  // maps a json object with a list of Capitalization-objects as value to a dart map
  static Map<String, List<Capitalization>> mapListFromJson(dynamic json, {bool emptyIsNull = false, bool growable = false,}) {
    final map = <String, List<Capitalization>>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = Capitalization.listFromJson(entry.value, emptyIsNull: emptyIsNull, growable: growable,);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }
}

