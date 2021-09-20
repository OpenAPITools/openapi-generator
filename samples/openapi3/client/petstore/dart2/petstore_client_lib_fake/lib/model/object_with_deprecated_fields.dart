//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class ObjectWithDeprecatedFields {
  /// Returns a new [ObjectWithDeprecatedFields] instance.
  ObjectWithDeprecatedFields({
    this.uuid,
    this.id,
    this.deprecatedRef,
    this.bars = const [],
  });

  String uuid;

  num id;

  DeprecatedObject deprecatedRef;

  List<String> bars;

  @override
  bool operator ==(Object other) => identical(this, other) || other is ObjectWithDeprecatedFields &&
     other.uuid == uuid &&
     other.id == id &&
     other.deprecatedRef == deprecatedRef &&
     other.bars == bars;

  @override
  int get hashCode =>
  // ignore: unnecessary_parenthesis
    (uuid == null ? 0 : uuid.hashCode) +
    (id == null ? 0 : id.hashCode) +
    (deprecatedRef == null ? 0 : deprecatedRef.hashCode) +
    (bars == null ? 0 : bars.hashCode);

  @override
  String toString() => 'ObjectWithDeprecatedFields[uuid=$uuid, id=$id, deprecatedRef=$deprecatedRef, bars=$bars]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (uuid != null) {
      json[r'uuid'] = uuid;
    }
    if (id != null) {
      json[r'id'] = id;
    }
    if (deprecatedRef != null) {
      json[r'deprecatedRef'] = deprecatedRef;
    }
    if (bars != null) {
      json[r'bars'] = bars;
    }
    return json;
  }

  /// Returns a new [ObjectWithDeprecatedFields] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static ObjectWithDeprecatedFields fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();
      return ObjectWithDeprecatedFields(
        uuid: mapValueOfType<String>(json, r'uuid'),
        id: json[r'id'] == null
          ? null
          : num.parse(json[r'id'].toString()),
        deprecatedRef: DeprecatedObject.fromJson(json[r'deprecatedRef']),
        bars: json[r'bars'] is List
          ? (json[r'bars'] as List).cast<String>()
          : null,
      );
    }
    return null;
  }

  static List<ObjectWithDeprecatedFields> listFromJson(dynamic json, {bool emptyIsNull, bool growable,}) =>
    json is List && json.isNotEmpty
      ? json.map(ObjectWithDeprecatedFields.fromJson).toList(growable: true == growable)
      : true == emptyIsNull ? null : <ObjectWithDeprecatedFields>[];

  static Map<String, ObjectWithDeprecatedFields> mapFromJson(dynamic json) {
    final map = <String, ObjectWithDeprecatedFields>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) => map[key] = ObjectWithDeprecatedFields.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of ObjectWithDeprecatedFields-objects as value to a dart map
  static Map<String, List<ObjectWithDeprecatedFields>> mapListFromJson(dynamic json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<ObjectWithDeprecatedFields>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = ObjectWithDeprecatedFields.listFromJson(
            value,
            emptyIsNull: emptyIsNull,
            growable: growable,
          );
        });
    }
    return map;
  }
}

