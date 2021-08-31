//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class Name {
  /// Returns a new [Name] instance.
  Name({
    @required this.name,
    this.snakeCase,
    this.property,
    this.n123number,
  });

  int name;

  int snakeCase;

  String property;

  int n123number;

  @override
  bool operator ==(Object other) => identical(this, other) || other is Name &&
     other.name == name &&
     other.snakeCase == snakeCase &&
     other.property == property &&
     other.n123number == n123number;

  @override
  int get hashCode =>
    (name == null ? 0 : name.hashCode) +
    (snakeCase == null ? 0 : snakeCase.hashCode) +
    (property == null ? 0 : property.hashCode) +
    (n123number == null ? 0 : n123number.hashCode);

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
  /// [json] if it's non-null, null if [json] is null.
  static Name fromJson(Map<String, dynamic> json) => json == null
    ? null
    : Name(
        name: json[r'name'],
        snakeCase: json[r'snake_case'],
        property: json[r'property'],
        n123number: json[r'123Number'],
    );

  static List<Name> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <Name>[]
      : json.map((dynamic value) => Name.fromJson(value)).toList(growable: true == growable);

  static Map<String, Name> mapFromJson(Map<String, dynamic> json) {
    final map = <String, Name>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) => map[key] = Name.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of Name-objects as value to a dart map
  static Map<String, List<Name>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<Name>>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) {
        map[key] = Name.listFromJson(value, emptyIsNull: emptyIsNull, growable: growable,);
      });
    }
    return map;
  }
}

