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
  static InlineResponseDefault fromJson(Map<String, dynamic> json) => json == null
    ? null
    : InlineResponseDefault(
        string: Foo.fromJson(json[r'string']),
    );

  static List<InlineResponseDefault> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <InlineResponseDefault>[]
      : json.map((dynamic value) => InlineResponseDefault.fromJson(value)).toList(growable: true == growable);

  static Map<String, InlineResponseDefault> mapFromJson(Map<String, dynamic> json) {
    final map = <String, InlineResponseDefault>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) => map[key] = InlineResponseDefault.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of InlineResponseDefault-objects as value to a dart map
  static Map<String, List<InlineResponseDefault>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<InlineResponseDefault>>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) {
        map[key] = InlineResponseDefault.listFromJson(value, emptyIsNull: emptyIsNull, growable: growable,);
      });
    }
    return map;
  }
}

