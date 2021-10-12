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


  String? smallCamel;

  String? capitalCamel;

  String? smallSnake;

  String? capitalSnake;

  String? sCAETHFlowPoints;

  /// Name of the pet 
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
    smallCamel.hashCode +
    capitalCamel.hashCode +
    smallSnake.hashCode +
    capitalSnake.hashCode +
    sCAETHFlowPoints.hashCode +
    ATT_NAME.hashCode;

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
  static Capitalization fromJson(Map<String, dynamic> json) => Capitalization(
        smallCamel: json[r'smallCamel'] as String,
        capitalCamel: json[r'CapitalCamel'] as String,
        smallSnake: json[r'small_Snake'] as String,
        capitalSnake: json[r'Capital_Snake'] as String,
        sCAETHFlowPoints: json[r'SCA_ETH_Flow_Points'] as String,
        ATT_NAME: json[r'ATT_NAME'] as String,
    );

  static List<Capitalization> listFromJson(List json, {bool? growable,}) =>
    json.isNotEmpty
      ? json.map<Capitalization>((i) => Capitalization.fromJson(i as Map<String, dynamic>)).toList(growable: true == growable)
      : <Capitalization>[];

  static Map<String, Capitalization> mapFromJson(dynamic json) {
    final map = <String, Capitalization>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) => map[key] = Capitalization.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of Capitalization-objects as value to a dart map
  static Map<String, List<Capitalization>> mapListFromJson(dynamic json, {bool? growable,}) {
    final map = <String, List<Capitalization>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = Capitalization.listFromJson(
            value,
            growable: growable,
          );
        });
    }
    return map;
  }
}

