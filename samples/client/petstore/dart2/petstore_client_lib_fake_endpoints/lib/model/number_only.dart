//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
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
    (justNumber == null ? 0 : justNumber.hashCode);

  @override
  String toString() => 'NumberOnly[justNumber=$justNumber]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (justNumber != null) {
      json['JustNumber'] = justNumber;
    }
    return json;
  }

  /// Returns a new [NumberOnly] instance and imports its values from
  /// [json] if it's non-null, null if [json] is null.
  static NumberOnly fromJson(Map<String, dynamic> json) => json == null
    ? null
    : NumberOnly(
        justNumber: json['JustNumber'] == null ?
          null :
          json['JustNumber'].toDouble(),
    );

  static List<NumberOnly> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <NumberOnly>[]
      : json.map((v) => NumberOnly.fromJson(v)).toList(growable: true == growable);

  static Map<String, NumberOnly> mapFromJson(Map<String, dynamic> json) {
    final map = <String, NumberOnly>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) => map[key] = NumberOnly.fromJson(v));
    }
    return map;
  }

  // maps a json object with a list of NumberOnly-objects as value to a dart map
  static Map<String, List<NumberOnly>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<NumberOnly>>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) {
        map[key] = NumberOnly.listFromJson(v, emptyIsNull: emptyIsNull, growable: growable);
      });
    }
    return map;
  }
}

