//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
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
  // ignore: unnecessary_parenthesis
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
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static Name fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();
      return Name(
        name: mapValueOfType<int>(json, r'name'),
        snakeCase: mapValueOfType<int>(json, r'snake_case'),
        property: mapValueOfType<String>(json, r'property'),
        n123number: mapValueOfType<int>(json, r'123Number'),
      );
    }
    return null;
  }

  static List<Name> listFromJson(dynamic json, {bool emptyIsNull, bool growable,}) =>
    json is List && json.isNotEmpty
      ? json.map(Name.fromJson).toList(growable: true == growable)
      : true == emptyIsNull ? null : <Name>[];

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
  static Map<String, List<Name>> mapListFromJson(dynamic json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<Name>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = Name.listFromJson(
            value,
            emptyIsNull: emptyIsNull,
            growable: growable,
          );
        });
    }
    return map;
  }
}

