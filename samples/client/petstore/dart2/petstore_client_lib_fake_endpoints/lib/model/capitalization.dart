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

  /// Returns a new [Capitalization] instance and optionally import its values from
  /// [json] if it's non-null.
  Capitalization.fromJson(Map<String, dynamic> json) {
    if (json != null) {
      smallCamel = json['smallCamel'];
      capitalCamel = json['CapitalCamel'];
      smallSnake = json['small_Snake'];
      capitalSnake = json['Capital_Snake'];
      sCAETHFlowPoints = json['SCA_ETH_Flow_Points'];
      ATT_NAME = json['ATT_NAME'];
    }
  }

  
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
      json['smallCamel'] = smallCamel;
    }
    if (capitalCamel != null) {
      json['CapitalCamel'] = capitalCamel;
    }
    if (smallSnake != null) {
      json['small_Snake'] = smallSnake;
    }
    if (capitalSnake != null) {
      json['Capital_Snake'] = capitalSnake;
    }
    if (sCAETHFlowPoints != null) {
      json['SCA_ETH_Flow_Points'] = sCAETHFlowPoints;
    }
    if (ATT_NAME != null) {
      json['ATT_NAME'] = ATT_NAME;
    }
    return json;
  }

  static List<Capitalization> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <Capitalization>[]
      : json.map((v) => Capitalization.fromJson(v)).toList(growable: true == growable);

  static Map<String, Capitalization> mapFromJson(Map<String, dynamic> json) {
    final map = <String, Capitalization>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) => map[key] = Capitalization.fromJson(v));
    }
    return map;
  }

  // maps a json object with a list of Capitalization-objects as value to a dart map
  static Map<String, List<Capitalization>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<Capitalization>>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) {
        map[key] = Capitalization.listFromJson(v, emptyIsNull: emptyIsNull, growable: growable);
      });
    }
    return map;
  }
}

