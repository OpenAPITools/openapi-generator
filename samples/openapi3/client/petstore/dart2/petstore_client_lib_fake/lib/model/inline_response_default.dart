//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.14

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class InlineResponseDefault {
  /// Returns a new [InlineResponseDefault] instance.
  InlineResponseDefault({
    this.string,
  });


  Foo? string;

  @override
  bool operator ==(Object other) => identical(this, other) || other is InlineResponseDefault &&
     other.string == string;

  @override
  int get hashCode =>
    string.hashCode;

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
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static InlineResponseDefault fromJson(Map<String, dynamic> json) => InlineResponseDefault(
        string: Foo.fromJson(json[r'string']),
    );

  static List<InlineResponseDefault> listFromJson(List json, {bool? growable,}) =>
    json.isNotEmpty
      ? json.map<InlineResponseDefault>((i) => InlineResponseDefault.fromJson(i as Map<String, dynamic>)).toList(growable: true == growable)
      : <InlineResponseDefault>[];

  static Map<String, InlineResponseDefault> mapFromJson(dynamic json) {
    final map = <String, InlineResponseDefault>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) => map[key] = InlineResponseDefault.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of InlineResponseDefault-objects as value to a dart map
  static Map<String, List<InlineResponseDefault>> mapListFromJson(dynamic json, {bool? growable,}) {
    final map = <String, List<InlineResponseDefault>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = InlineResponseDefault.listFromJson(
            value,
            growable: growable,
          );
        });
    }
    return map;
  }
}

