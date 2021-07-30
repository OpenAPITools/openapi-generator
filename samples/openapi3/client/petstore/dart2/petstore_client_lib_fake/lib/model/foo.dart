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

  String bar;

  @override
  bool operator ==(Object other) => identical(this, other) || other is Foo &&
     other.bar == bar;

  @override
  int get hashCode =>
    (bar == null ? 0 : bar.hashCode);

  @override
  String toString() => 'Foo[bar=$bar]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (bar != null) {
      json[r'bar'] = bar;
    }
    return json;
  }

  /// Returns a new [Foo] instance and imports its values from
  /// [json] if it's non-null, null if [json] is null.
  static Foo fromJson(Map<String, dynamic> json) => json == null
    ? null
    : Foo(
        bar: json[r'bar'],
    );

  static List<Foo> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <Foo>[]
      : json.map((dynamic value) => Foo.fromJson(value)).toList(growable: true == growable);

  static Map<String, Foo> mapFromJson(Map<String, dynamic> json) {
    final map = <String, Foo>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) => map[key] = Foo.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of Foo-objects as value to a dart map
  static Map<String, List<Foo>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<Foo>>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) {
        map[key] = Foo.listFromJson(value, emptyIsNull: emptyIsNull, growable: growable,);
      });
    }
    return map;
  }
}

