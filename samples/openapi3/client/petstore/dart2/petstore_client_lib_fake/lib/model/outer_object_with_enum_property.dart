//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.12

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class OuterObjectWithEnumProperty {
  /// Returns a new [OuterObjectWithEnumProperty] instance.
  OuterObjectWithEnumProperty({
    required this.value,
  });


  OuterEnumInteger value;

  @override
  bool operator ==(Object other) => identical(this, other) || other is OuterObjectWithEnumProperty &&
     other.value == value;

  @override
  int get hashCode =>
    value.hashCode;

  @override
  String toString() => 'OuterObjectWithEnumProperty[value=$value]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
      json[r'value'] = value;
    return json;
  }

  /// Returns a new [OuterObjectWithEnumProperty] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static OuterObjectWithEnumProperty fromJson(Map<String, dynamic> json) => OuterObjectWithEnumProperty(
        value: OuterEnumInteger.fromJson(json[r'value']),
    );

  static List<OuterObjectWithEnumProperty> listFromJson(List json, {bool? growable,}) =>
    json.isNotEmpty
      ? json.map<OuterObjectWithEnumProperty>((i) => OuterObjectWithEnumProperty.fromJson(i as Map<String, dynamic>)).toList(growable: true == growable)
      : <OuterObjectWithEnumProperty>[];

  static Map<String, OuterObjectWithEnumProperty> mapFromJson(dynamic json) {
    final map = <String, OuterObjectWithEnumProperty>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) => map[key] = OuterObjectWithEnumProperty.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of OuterObjectWithEnumProperty-objects as value to a dart map
  static Map<String, List<OuterObjectWithEnumProperty>> mapListFromJson(dynamic json, {bool? growable,}) {
    final map = <String, List<OuterObjectWithEnumProperty>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = OuterObjectWithEnumProperty.listFromJson(
            value,
            growable: growable,
          );
        });
    }
    return map;
  }
}

