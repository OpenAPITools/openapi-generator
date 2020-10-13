//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class Foo {
  /// Returns a new [Foo] instance.
  Foo({
    this.bar = 'bar',
  });

  /// Returns a new [Foo] instance and optionally import its values from
  /// [json] if it's non-null.
  Foo.fromJson(Map<String, dynamic> json) {
    if (json != null) {
      bar = json['bar'];
    }
  }

  
  String bar;

  @override
  bool operator ==(Object other) => identical(this, other) || other is Foo &&
     other.bar == bar;

  @override
  int get hashCode =>
    bar.hashCode;

  @override
  String toString() => 'Foo[bar=$bar]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (bar != null) {
      json['bar'] = bar;
    }
    return json;
  }

  static List<Foo> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <Foo>[]
      : json.map((v) => Foo.fromJson(v)).toList(growable: true == growable);

  static Map<String, Foo> mapFromJson(Map<String, dynamic> json) {
    final map = <String, Foo>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) => map[key] = Foo.fromJson(v));
    }
    return map;
  }

  // maps a json object with a list of Foo-objects as value to a dart map
  static Map<String, List<Foo>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<Foo>>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) {
        map[key] = Foo.listFromJson(v, emptyIsNull: emptyIsNull, growable: growable);
      });
    }
    return map;
  }
}

