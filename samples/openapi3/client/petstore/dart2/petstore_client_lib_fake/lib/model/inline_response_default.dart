//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class InlineResponseDefault {
  /// Returns a new [InlineResponseDefault] instance.
  InlineResponseDefault({
    this.string,
  });

  Foo string;

  @override
  bool operator ==(Object other) => identical(this, other) || other is InlineResponseDefault &&
     other.string == string;

  @override
  int get hashCode =>
  // ignore: unnecessary_parenthesis
    (string == null ? 0 : string.hashCode);

  @override
  String toString() => 'InlineResponseDefault[string=$string]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (string != null) {
      json[r'string'] = string;
    }
    return json;
  }

  /// Returns a new [InlineResponseDefault] instance and imports its values from
  /// [json] if it's non-null, null if [json] is null.
  // ignore: prefer_constructors_over_static_methods
  static InlineResponseDefault fromJson(Map<String, dynamic> json) => json == null
    ? null
    : InlineResponseDefault(
        string: json[r'string'] is Map
          ? Foo.fromJson((json[r'string'] as Map).cast<String, dynamic>())
          : null,
    );

  static List<InlineResponseDefault> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <InlineResponseDefault>[]
      : json
          .map((dynamic value) => InlineResponseDefault.fromJson((value as Map).cast<String, dynamic>()))
          .toList(growable: true == growable);

  static Map<String, InlineResponseDefault> mapFromJson(Map<String, dynamic> json) {
    final map = <String, InlineResponseDefault>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, dynamic value) => map[key] = InlineResponseDefault.fromJson((value as Map).cast<String, dynamic>()));
    }
    return map;
  }

  // maps a json object with a list of InlineResponseDefault-objects as value to a dart map
  static Map<String, List<InlineResponseDefault>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<InlineResponseDefault>>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, dynamic value) {
        map[key] = InlineResponseDefault.listFromJson(value as List, emptyIsNull: emptyIsNull, growable: growable,);
      });
    }
    return map;
  }
}

