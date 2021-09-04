//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

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

  num justNumber;

  @override
  bool operator ==(Object other) => identical(this, other) || other is NumberOnly &&
     other.justNumber == justNumber;

  @override
  int get hashCode =>
  // ignore: unnecessary_parenthesis
    (justNumber == null ? 0 : justNumber.hashCode);

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
  static NumberOnly fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();
      return NumberOnly(
        justNumber: json[r'JustNumber'] == null
          ? null
          : num.parse(json[r'JustNumber'].toString()),
      );
    }
    return null;
  }

  static List<NumberOnly> listFromJson(dynamic json, {bool emptyIsNull, bool growable,}) =>
    json is List && json.isNotEmpty
      ? json.map(NumberOnly.fromJson).toList(growable: true == growable)
      : true == emptyIsNull ? null : <NumberOnly>[];

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
  static Map<String, List<NumberOnly>> mapListFromJson(dynamic json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<NumberOnly>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = NumberOnly.listFromJson(
            value,
            emptyIsNull: emptyIsNull,
            growable: growable,
          );
        });
    }
    return map;
  }
}

