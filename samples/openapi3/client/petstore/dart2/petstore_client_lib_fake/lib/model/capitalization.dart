//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.12

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

  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  String? smallCamel;

  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  String? capitalCamel;

  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  String? smallSnake;

  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  String? capitalSnake;

  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  String? sCAETHFlowPoints;

  /// Name of the pet 
  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  String? ATT_NAME;

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
    (smallCamel == null ? 0 : smallCamel!.hashCode) +
    (capitalCamel == null ? 0 : capitalCamel!.hashCode) +
    (smallSnake == null ? 0 : smallSnake!.hashCode) +
    (capitalSnake == null ? 0 : capitalSnake!.hashCode) +
    (sCAETHFlowPoints == null ? 0 : sCAETHFlowPoints!.hashCode) +
    (ATT_NAME == null ? 0 : ATT_NAME!.hashCode);

  @override
  String toString() => 'Capitalization[smallCamel=$smallCamel, capitalCamel=$capitalCamel, smallSnake=$smallSnake, capitalSnake=$capitalSnake, sCAETHFlowPoints=$sCAETHFlowPoints, ATT_NAME=$ATT_NAME]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (smallCamel != null) {
      json[r'smallCamel'] = smallCamel;
    }
    if (capitalCamel != null) {
      json[r'CapitalCamel'] = capitalCamel;
    }
    if (smallSnake != null) {
      json[r'small_Snake'] = smallSnake;
    }
    if (capitalSnake != null) {
      json[r'Capital_Snake'] = capitalSnake;
    }
    if (sCAETHFlowPoints != null) {
      json[r'SCA_ETH_Flow_Points'] = sCAETHFlowPoints;
    }
    if (ATT_NAME != null) {
      json[r'ATT_NAME'] = ATT_NAME;
    }
    return json;
  }

  /// Returns a new [Capitalization] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static Capitalization? fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();

      // Ensure that the map contains the required keys.
      // Note 1: the values aren't checked for validity beyond being non-null.
      // Note 2: this code is stripped in release mode!
      assert(() {
        requiredKeys.forEach((key) {
          assert(json.containsKey(key), 'Required key "Capitalization[$key]" is missing from JSON.');
          assert(json[key] != null, 'Required key "Capitalization[$key]" has a null value in JSON.');
        });
        return true;
      }());

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

  static List<Capitalization>? listFromJson(dynamic json, {bool growable = false,}) {
    final result = <Capitalization>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = Capitalization.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
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
  static Map<String, List<Capitalization>> mapListFromJson(dynamic json, {bool growable = false,}) {
    final map = <String, List<Capitalization>>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = Capitalization.listFromJson(entry.value, growable: growable,);
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

