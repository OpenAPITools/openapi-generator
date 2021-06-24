//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
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
    (smallCamel == null ? 0 : smallCamel.hashCode) +
    (capitalCamel == null ? 0 : capitalCamel.hashCode) +
    (smallSnake == null ? 0 : smallSnake.hashCode) +
    (capitalSnake == null ? 0 : capitalSnake.hashCode) +
    (sCAETHFlowPoints == null ? 0 : sCAETHFlowPoints.hashCode) +
    (ATT_NAME == null ? 0 : ATT_NAME.hashCode);

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
  /// [json] if it's non-null, null if [json] is null.
  static Capitalization fromJson(Map<String, dynamic> json) => json == null
    ? null
    : Capitalization(
        smallCamel: json[r'smallCamel'],
        capitalCamel: json[r'CapitalCamel'],
        smallSnake: json[r'small_Snake'],
        capitalSnake: json[r'Capital_Snake'],
        sCAETHFlowPoints: json[r'SCA_ETH_Flow_Points'],
        ATT_NAME: json[r'ATT_NAME'],
    );

  static List<Capitalization> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <Capitalization>[]
      : json.map((dynamic value) => Capitalization.fromJson(value)).toList(growable: true == growable);

  static Map<String, Capitalization> mapFromJson(Map<String, dynamic> json) {
    final map = <String, Capitalization>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) => map[key] = Capitalization.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of Capitalization-objects as value to a dart map
  static Map<String, List<Capitalization>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<Capitalization>>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) {
        map[key] = Capitalization.listFromJson(value, emptyIsNull: emptyIsNull, growable: growable,);
      });
    }
    return map;
  }
}

