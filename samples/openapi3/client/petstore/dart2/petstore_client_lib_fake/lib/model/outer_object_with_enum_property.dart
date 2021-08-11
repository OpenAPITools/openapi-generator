//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
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
    (value == null ? 0 : value.hashCode);

  @override
  String toString() => 'OuterObjectWithEnumProperty[value=$value]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
      json[r'value'] = value;
    return json;
  }

  /// Returns a new [OuterObjectWithEnumProperty] instance and imports its values from
  /// [json] if it's non-null, null if [json] is null.
  static OuterObjectWithEnumProperty fromJson(Map<String, dynamic> json) => json == null
    ? null
    : OuterObjectWithEnumProperty(
        value: OuterEnumInteger.fromJson(json[r'value']),
    );

  static List<OuterObjectWithEnumProperty> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <OuterObjectWithEnumProperty>[]
      : json.map((dynamic value) => OuterObjectWithEnumProperty.fromJson(value)).toList(growable: true == growable);

  static Map<String, OuterObjectWithEnumProperty> mapFromJson(Map<String, dynamic> json) {
    final map = <String, OuterObjectWithEnumProperty>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) => map[key] = OuterObjectWithEnumProperty.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of OuterObjectWithEnumProperty-objects as value to a dart map
  static Map<String, List<OuterObjectWithEnumProperty>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<OuterObjectWithEnumProperty>>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) {
        map[key] = OuterObjectWithEnumProperty.listFromJson(value, emptyIsNull: emptyIsNull, growable: growable,);
      });
    }
    return map;
  }
}

