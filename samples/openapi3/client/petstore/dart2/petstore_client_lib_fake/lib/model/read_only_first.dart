//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.14

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


  String? bar;

  String? baz;

  @override
  bool operator ==(Object other) => identical(this, other) || other is ReadOnlyFirst &&
     other.bar == bar &&
     other.baz == baz;

  @override
  int get hashCode =>
    bar.hashCode +
    baz.hashCode;

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
  static ReadOnlyFirst fromJson(Map<String, dynamic> json) => ReadOnlyFirst(
        bar: json[r'bar'] as String,
        baz: json[r'baz'] as String,
    );

  static List<ReadOnlyFirst> listFromJson(List json, {bool? growable,}) =>
    json.isNotEmpty
      ? json.map<ReadOnlyFirst>((i) => ReadOnlyFirst.fromJson(i as Map<String, dynamic>)).toList(growable: true == growable)
      : <ReadOnlyFirst>[];

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
  static Map<String, List<ReadOnlyFirst>> mapListFromJson(dynamic json, {bool? growable,}) {
    final map = <String, List<ReadOnlyFirst>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = ReadOnlyFirst.listFromJson(
            value,
            growable: growable,
          );
        });
    }
    return map;
  }
}

