//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.14

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class Name {
  /// Returns a new [Name] instance.
  Name({
    required this.name,
    this.snakeCase,
    this.property,
    this.n123number,
  });


  int name;

  int? snakeCase;

  String? property;

  int? n123number;

  @override
  bool operator ==(Object other) => identical(this, other) || other is Name &&
     other.name == name &&
     other.snakeCase == snakeCase &&
     other.property == property &&
     other.n123number == n123number;

  @override
  int get hashCode =>
    name.hashCode +
    snakeCase.hashCode +
    property.hashCode +
    n123number.hashCode;

  @override
  String toString() => 'Name[name=$name, snakeCase=$snakeCase, property=$property, n123number=$n123number]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
      json[r'name'] = name;
    if (snakeCase != null) {
      json[r'snake_case'] = snakeCase;
    }
    if (property != null) {
      json[r'property'] = property;
    }
    if (n123number != null) {
      json[r'123Number'] = n123number;
    }
    return json;
  }

  /// Returns a new [Name] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static Name fromJson(Map<String, dynamic> json) => Name(
        name: json[r'name'] as int,
        snakeCase: json[r'snake_case'] as int,
        property: json[r'property'] as String,
        n123number: json[r'123Number'] as int,
    );

  static List<Name> listFromJson(List json, {bool? growable,}) =>
    json.isNotEmpty
      ? json.map<Name>((i) => Name.fromJson(i as Map<String, dynamic>)).toList(growable: true == growable)
      : <Name>[];

  static Map<String, Name> mapFromJson(dynamic json) {
    final map = <String, Name>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) => map[key] = Name.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of Name-objects as value to a dart map
  static Map<String, List<Name>> mapListFromJson(dynamic json, {bool? growable,}) {
    final map = <String, List<Name>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = Name.listFromJson(
            value,
            growable: growable,
          );
        });
    }
    return map;
  }
}

