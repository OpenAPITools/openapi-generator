//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

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

  String bar;

  @override
  bool operator ==(Object other) => identical(this, other) || other is Foo &&
     other.bar == bar;

  @override
  int get hashCode =>
  // ignore: unnecessary_parenthesis
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
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static Foo fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();
      return Foo(
        bar: mapValueOfType<String>(json, r'bar'),
      );
    }
    return null;
  }

  static List<Foo> listFromJson(dynamic json, {bool emptyIsNull, bool growable,}) =>
    json is List && json.isNotEmpty
      ? json.map(Foo.fromJson).toList(growable: true == growable)
      : true == emptyIsNull ? null : <Foo>[];

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
  static Map<String, List<Foo>> mapListFromJson(dynamic json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<Foo>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = Foo.listFromJson(
            value,
            emptyIsNull: emptyIsNull,
            growable: growable,
          );
        });
    }
    return map;
  }
}

