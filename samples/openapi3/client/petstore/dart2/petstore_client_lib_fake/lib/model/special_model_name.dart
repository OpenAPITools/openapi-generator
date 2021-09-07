//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
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
    (dollarSpecialLeftSquareBracketPropertyPeriodNameRightSquareBracket == null ? 0 : dollarSpecialLeftSquareBracketPropertyPeriodNameRightSquareBracket.hashCode);

  @override
  String toString() => 'SpecialModelName[dollarSpecialLeftSquareBracketPropertyPeriodNameRightSquareBracket=$dollarSpecialLeftSquareBracketPropertyPeriodNameRightSquareBracket]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (dollarSpecialLeftSquareBracketPropertyPeriodNameRightSquareBracket != null) {
      json[r'$special[property.name]'] = dollarSpecialLeftSquareBracketPropertyPeriodNameRightSquareBracket;
    }
    return json;
  }

  /// Returns a new [SpecialModelName] instance and imports its values from
  /// [json] if it's non-null, null if [json] is null.
  static SpecialModelName fromJson(Map<String, dynamic> json) => json == null
    ? null
    : SpecialModelName(
        dollarSpecialLeftSquareBracketPropertyPeriodNameRightSquareBracket: json[r'$special[property.name]'],
    );

  static List<SpecialModelName> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <SpecialModelName>[]
      : json.map((dynamic value) => SpecialModelName.fromJson(value)).toList(growable: true == growable);

  static Map<String, SpecialModelName> mapFromJson(Map<String, dynamic> json) {
    final map = <String, SpecialModelName>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) => map[key] = SpecialModelName.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of SpecialModelName-objects as value to a dart map
  static Map<String, List<SpecialModelName>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<SpecialModelName>>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) {
        map[key] = SpecialModelName.listFromJson(value, emptyIsNull: emptyIsNull, growable: growable,);
      });
    }
    return map;
  }
}

