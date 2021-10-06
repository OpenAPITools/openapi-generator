//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.14

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class Foo {
  /// Returns a new [Foo] instance.
  Foo({
    this.bar = 'bar',
  });


  String? bar;

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
      json[r'bar'] = bar;
    }
    return json;
  }

  /// Returns a new [Foo] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static Foo fromJson(Map<String, dynamic> json) => Foo(
        bar: json[r'bar'] as String,
    );

  static List<Foo> listFromJson(List json, {bool? growable,}) =>
    json.isNotEmpty
      ? json.map<Foo>((i) => Foo.fromJson(i as Map<String, dynamic>)).toList(growable: true == growable)
      : <Foo>[];

  static Map<String, Foo> mapFromJson(dynamic json) {
    final map = <String, Foo>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) => map[key] = Foo.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of Foo-objects as value to a dart map
  static Map<String, List<Foo>> mapListFromJson(dynamic json, {bool? growable,}) {
    final map = <String, List<Foo>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = Foo.listFromJson(
            value,
            growable: growable,
          );
        });
    }
    return map;
  }
}

