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

  /// Returns a new [ReadOnlyFirst] instance and optionally import its values from
  /// [json] if it's non-null.
  ReadOnlyFirst.fromJson(Map<String, dynamic> json) {
    if (json != null) {
      bar = json['bar'];
      baz = json['baz'];
    }
  }

  
  String bar;

  
  String baz;

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
      json['bar'] = bar;
    }
    if (baz != null) {
      json['baz'] = baz;
    }
    return json;
  }

  static List<ReadOnlyFirst> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <ReadOnlyFirst>[]
      : json.map((v) => ReadOnlyFirst.fromJson(v)).toList(growable: true == growable);

  static Map<String, ReadOnlyFirst> mapFromJson(Map<String, dynamic> json) {
    final map = <String, ReadOnlyFirst>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) => map[key] = ReadOnlyFirst.fromJson(v));
    }
    return map;
  }

  // maps a json object with a list of ReadOnlyFirst-objects as value to a dart map
  static Map<String, List<ReadOnlyFirst>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<ReadOnlyFirst>>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) {
        map[key] = ReadOnlyFirst.listFromJson(v, emptyIsNull: emptyIsNull, growable: growable);
      });
    }
    return map;
  }
}

