//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

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
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static SpecialModelName fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();
      return SpecialModelName(
        dollarSpecialLeftSquareBracketPropertyPeriodNameRightSquareBracket: mapValueOfType<int>(json, r'$special[property.name]'),
      );
    }
    return null;
  }

  static List<SpecialModelName> listFromJson(dynamic json, {bool emptyIsNull, bool growable,}) =>
    json is List && json.isNotEmpty
      ? json.map(SpecialModelName.fromJson).toList(growable: true == growable)
      : true == emptyIsNull ? null : <SpecialModelName>[];

  static Map<String, SpecialModelName> mapFromJson(dynamic json) {
    final map = <String, SpecialModelName>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) => map[key] = SpecialModelName.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of SpecialModelName-objects as value to a dart map
  static Map<String, List<SpecialModelName>> mapListFromJson(dynamic json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<SpecialModelName>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = SpecialModelName.listFromJson(
            value,
            emptyIsNull: emptyIsNull,
            growable: growable,
          );
        });
    }
    return map;
  }
}

