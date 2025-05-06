//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.18

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class Capitalization {
  /// Returns a new [Capitalization] instance.
  Capitalization({
    this.smallcamel,
    this.capitalcamel,
    this.smallSnake,
    this.capitalSnake,
    this.scaEthFlowPoints,
    this.attName,
  });

  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  String? smallcamel;

  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  String? capitalcamel;

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
  String? scaEthFlowPoints;

  /// Name of the pet 
  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  String? attName;

  @override
  bool operator ==(Object other) => identical(this, other) || other is Capitalization &&
    other.smallcamel == smallcamel &&
    other.capitalcamel == capitalcamel &&
    other.smallSnake == smallSnake &&
    other.capitalSnake == capitalSnake &&
    other.scaEthFlowPoints == scaEthFlowPoints &&
    other.attName == attName;

  @override
  int get hashCode =>
    // ignore: unnecessary_parenthesis
    (smallcamel == null ? 0 : smallcamel!.hashCode) +
    (capitalcamel == null ? 0 : capitalcamel!.hashCode) +
    (smallSnake == null ? 0 : smallSnake!.hashCode) +
    (capitalSnake == null ? 0 : capitalSnake!.hashCode) +
    (scaEthFlowPoints == null ? 0 : scaEthFlowPoints!.hashCode) +
    (attName == null ? 0 : attName!.hashCode);

  @override
  String toString() => 'Capitalization[smallcamel=$smallcamel, capitalcamel=$capitalcamel, smallSnake=$smallSnake, capitalSnake=$capitalSnake, scaEthFlowPoints=$scaEthFlowPoints, attName=$attName]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (this.smallcamel != null) {
      json[r'smallCamel'] = this.smallcamel;
    } else {
      json[r'smallCamel'] = null;
    }
    if (this.capitalcamel != null) {
      json[r'CapitalCamel'] = this.capitalcamel;
    } else {
      json[r'CapitalCamel'] = null;
    }
    if (this.smallSnake != null) {
      json[r'small_Snake'] = this.smallSnake;
    } else {
      json[r'small_Snake'] = null;
    }
    if (this.capitalSnake != null) {
      json[r'Capital_Snake'] = this.capitalSnake;
    } else {
      json[r'Capital_Snake'] = null;
    }
    if (this.scaEthFlowPoints != null) {
      json[r'SCA_ETH_Flow_Points'] = this.scaEthFlowPoints;
    } else {
      json[r'SCA_ETH_Flow_Points'] = null;
    }
    if (this.attName != null) {
      json[r'ATT_NAME'] = this.attName;
    } else {
      json[r'ATT_NAME'] = null;
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
        smallcamel: mapValueOfType<String>(json, r'smallCamel'),
        capitalcamel: mapValueOfType<String>(json, r'CapitalCamel'),
        smallSnake: mapValueOfType<String>(json, r'small_Snake'),
        capitalSnake: mapValueOfType<String>(json, r'Capital_Snake'),
        scaEthFlowPoints: mapValueOfType<String>(json, r'SCA_ETH_Flow_Points'),
        attName: mapValueOfType<String>(json, r'ATT_NAME'),
      );
    }
    return null;
  }

  static List<Capitalization> listFromJson(dynamic json, {bool growable = false,}) {
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
      // ignore: parameter_assignments
      json = json.cast<String, dynamic>();
      for (final entry in json.entries) {
        map[entry.key] = Capitalization.listFromJson(entry.value, growable: growable,);
      }
    }
    return map;
  }

  /// The list of required keys that must be present in a JSON.
  static const requiredKeys = <String>{
  };
}

