//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
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
  /// [json] if it's non-null, null if [json] is null.
  static HasOnlyReadOnly fromJson(Map<String, dynamic> json) => json == null
    ? null
    : HasOnlyReadOnly(
        bar: json[r'bar'],
        foo: json[r'foo'],
    );

  static List<HasOnlyReadOnly> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <HasOnlyReadOnly>[]
      : json.map((dynamic value) => HasOnlyReadOnly.fromJson(value)).toList(growable: true == growable);

  static Map<String, HasOnlyReadOnly> mapFromJson(Map<String, dynamic> json) {
    final map = <String, HasOnlyReadOnly>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) => map[key] = HasOnlyReadOnly.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of HasOnlyReadOnly-objects as value to a dart map
  static Map<String, List<HasOnlyReadOnly>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<HasOnlyReadOnly>>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) {
        map[key] = HasOnlyReadOnly.listFromJson(value, emptyIsNull: emptyIsNull, growable: growable,);
      });
    }
    return map;
  }
}

