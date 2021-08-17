//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class ReadOnlyFirst {
  /// Returns a new [ReadOnlyFirst] instance.
  ReadOnlyFirst({
    this.bar,
    this.baz,
  });

  String bar;

  String baz;

  @override
  bool operator ==(Object other) => identical(this, other) || other is ReadOnlyFirst &&
     other.bar == bar &&
     other.baz == baz;

  @override
  int get hashCode =>
    (bar == null ? 0 : bar.hashCode) +
    (baz == null ? 0 : baz.hashCode);

  @override
  String toString() => 'ReadOnlyFirst[bar=$bar, baz=$baz]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (bar != null) {
      json[r'bar'] = bar;
    }
    if (baz != null) {
      json[r'baz'] = baz;
    }
    return json;
  }

  /// Returns a new [ReadOnlyFirst] instance and imports its values from
  /// [json] if it's non-null, null if [json] is null.
  static ReadOnlyFirst fromJson(Map<String, dynamic> json) => json == null
    ? null
    : ReadOnlyFirst(
        bar: json[r'bar'],
        baz: json[r'baz'],
    );

  static List<ReadOnlyFirst> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <ReadOnlyFirst>[]
      : json.map((dynamic value) => ReadOnlyFirst.fromJson(value)).toList(growable: true == growable);

  static Map<String, ReadOnlyFirst> mapFromJson(Map<String, dynamic> json) {
    final map = <String, ReadOnlyFirst>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) => map[key] = ReadOnlyFirst.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of ReadOnlyFirst-objects as value to a dart map
  static Map<String, List<ReadOnlyFirst>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<ReadOnlyFirst>>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) {
        map[key] = ReadOnlyFirst.listFromJson(value, emptyIsNull: emptyIsNull, growable: growable,);
      });
    }
    return map;
  }
}

