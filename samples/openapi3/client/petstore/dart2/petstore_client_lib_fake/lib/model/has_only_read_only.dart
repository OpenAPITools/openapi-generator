//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

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

  String bar;

  String foo;

  @override
  bool operator ==(Object other) => identical(this, other) || other is HasOnlyReadOnly &&
     other.bar == bar &&
     other.foo == foo;

  @override
  int get hashCode =>
  // ignore: unnecessary_parenthesis
    (bar == null ? 0 : bar.hashCode) +
    (foo == null ? 0 : foo.hashCode);

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
  static HasOnlyReadOnly fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();
      return HasOnlyReadOnly(
        bar: mapValueOfType<String>(json, r'bar'),
        foo: mapValueOfType<String>(json, r'foo'),
      );
    }
    return null;
  }

  static List<HasOnlyReadOnly> listFromJson(dynamic json, {bool emptyIsNull, bool growable,}) =>
    json is List && json.isNotEmpty
      ? json.map(HasOnlyReadOnly.fromJson).toList(growable: true == growable)
      : true == emptyIsNull ? null : <HasOnlyReadOnly>[];

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
  static Map<String, List<HasOnlyReadOnly>> mapListFromJson(dynamic json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<HasOnlyReadOnly>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = HasOnlyReadOnly.listFromJson(
            value,
            emptyIsNull: emptyIsNull,
            growable: growable,
          );
        });
    }
    return map;
  }
}

