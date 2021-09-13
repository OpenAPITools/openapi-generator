//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class OuterObjectWithEnumProperty {
  /// Returns a new [OuterObjectWithEnumProperty] instance.
  OuterObjectWithEnumProperty({
    @required this.value,
  });

  OuterEnumInteger value;

  @override
  bool operator ==(Object other) => identical(this, other) || other is OuterObjectWithEnumProperty &&
     other.value == value;

  @override
  int get hashCode =>
  // ignore: unnecessary_parenthesis
    (value == null ? 0 : value.hashCode);

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
  static OuterObjectWithEnumProperty fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();
      return OuterObjectWithEnumProperty(
        value: OuterEnumInteger.fromJson(json[r'value']),
      );
    }
    return null;
  }

  static List<OuterObjectWithEnumProperty> listFromJson(dynamic json, {bool emptyIsNull, bool growable,}) =>
    json is List && json.isNotEmpty
      ? json.map(OuterObjectWithEnumProperty.fromJson).toList(growable: true == growable)
      : true == emptyIsNull ? null : <OuterObjectWithEnumProperty>[];

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
  static Map<String, List<OuterObjectWithEnumProperty>> mapListFromJson(dynamic json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<OuterObjectWithEnumProperty>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = OuterObjectWithEnumProperty.listFromJson(
            value,
            emptyIsNull: emptyIsNull,
            growable: growable,
          );
        });
    }
    return map;
  }
}

