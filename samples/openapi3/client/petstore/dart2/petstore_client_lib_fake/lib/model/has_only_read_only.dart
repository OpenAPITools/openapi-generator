//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.12

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class HasOnlyReadOnly {
  /// Returns a new [HasOnlyReadOnly] instance.
  HasOnlyReadOnly({
    this.bar,
    this.foo,
  });


  String? bar;

  String? foo;

  @override
  bool operator ==(Object other) => identical(this, other) || other is HasOnlyReadOnly &&
     other.bar == bar &&
     other.foo == foo;

  @override
  int get hashCode =>
    bar.hashCode +
    foo.hashCode;

  @override
  String toString() => 'HasOnlyReadOnly[bar=$bar, foo=$foo]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (bar != null) {
      json[r'bar'] = bar;
    }
    if (foo != null) {
      json[r'foo'] = foo;
    }
    return json;
  }

  /// Returns a new [HasOnlyReadOnly] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static HasOnlyReadOnly fromJson(Map<String, dynamic> json) => HasOnlyReadOnly(
        bar: json[r'bar'] as String,
        foo: json[r'foo'] as String,
    );

  static List<HasOnlyReadOnly> listFromJson(List json, {bool? growable,}) =>
    json.isNotEmpty
      ? json.map<HasOnlyReadOnly>((i) => HasOnlyReadOnly.fromJson(i as Map<String, dynamic>)).toList(growable: true == growable)
      : <HasOnlyReadOnly>[];

  static Map<String, HasOnlyReadOnly> mapFromJson(dynamic json) {
    final map = <String, HasOnlyReadOnly>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) => map[key] = HasOnlyReadOnly.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of HasOnlyReadOnly-objects as value to a dart map
  static Map<String, List<HasOnlyReadOnly>> mapListFromJson(dynamic json, {bool? growable,}) {
    final map = <String, List<HasOnlyReadOnly>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = HasOnlyReadOnly.listFromJson(
            value,
            growable: growable,
          );
        });
    }
    return map;
  }
}

