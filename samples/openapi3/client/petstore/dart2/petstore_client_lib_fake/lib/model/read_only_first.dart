//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
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
  // ignore: unnecessary_parenthesis
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
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static ReadOnlyFirst fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();
      return ReadOnlyFirst(
        bar: mapValueOfType<String>(json, r'bar'),
        baz: mapValueOfType<String>(json, r'baz'),
      );
    }
    return null;
  }

  static List<ReadOnlyFirst> listFromJson(dynamic json, {bool emptyIsNull, bool growable,}) =>
    json is List && json.isNotEmpty
      ? json.map(ReadOnlyFirst.fromJson).toList(growable: true == growable)
      : true == emptyIsNull ? null : <ReadOnlyFirst>[];

  static Map<String, ReadOnlyFirst> mapFromJson(dynamic json) {
    final map = <String, ReadOnlyFirst>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) => map[key] = ReadOnlyFirst.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of ReadOnlyFirst-objects as value to a dart map
  static Map<String, List<ReadOnlyFirst>> mapListFromJson(dynamic json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<ReadOnlyFirst>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = ReadOnlyFirst.listFromJson(
            value,
            emptyIsNull: emptyIsNull,
            growable: growable,
          );
        });
    }
    return map;
  }
}

