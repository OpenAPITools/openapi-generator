//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.12

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class NumberOnly {
  /// Returns a new [NumberOnly] instance.
  NumberOnly({
    this.justNumber,
  });


  num? justNumber;

  @override
  bool operator ==(Object other) => identical(this, other) || other is NumberOnly &&
     other.justNumber == justNumber;

  @override
  int get hashCode =>
    justNumber.hashCode;

  @override
  String toString() => 'NumberOnly[justNumber=$justNumber]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (justNumber != null) {
      json[r'JustNumber'] = justNumber;
    }
    return json;
  }

  /// Returns a new [NumberOnly] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static NumberOnly fromJson(Map<String, dynamic> json) => NumberOnly(
        justNumber: json[r'JustNumber'] as num,
    );

  static List<NumberOnly> listFromJson(List json, {bool? growable,}) =>
    json.isNotEmpty
      ? json.map<NumberOnly>((i) => NumberOnly.fromJson(i as Map<String, dynamic>)).toList(growable: true == growable)
      : <NumberOnly>[];

  static Map<String, NumberOnly> mapFromJson(dynamic json) {
    final map = <String, NumberOnly>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) => map[key] = NumberOnly.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of NumberOnly-objects as value to a dart map
  static Map<String, List<NumberOnly>> mapListFromJson(dynamic json, {bool? growable,}) {
    final map = <String, List<NumberOnly>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = NumberOnly.listFromJson(
            value,
            growable: growable,
          );
        });
    }
    return map;
  }
}

