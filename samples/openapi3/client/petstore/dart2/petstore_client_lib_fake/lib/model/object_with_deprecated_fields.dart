//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.12

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


  String? uuid;

  num? id;

  DeprecatedObject? deprecatedRef;

  List<String>? bars;

  @override
  bool operator ==(Object other) => identical(this, other) || other is ObjectWithDeprecatedFields &&
     other.uuid == uuid &&
     other.id == id &&
     other.deprecatedRef == deprecatedRef &&
     other.bars == bars;

  @override
  int get hashCode =>
    uuid.hashCode +
    id.hashCode +
    deprecatedRef.hashCode +
    bars.hashCode;

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
  static ObjectWithDeprecatedFields fromJson(Map<String, dynamic> json) => ObjectWithDeprecatedFields(
        uuid: json[r'uuid'] as String,
        id: json[r'id'] as num,
        deprecatedRef: DeprecatedObject.fromJson(json[r'deprecatedRef']),
        bars: json[r'bars'] is List
          ? (json[r'bars'] as List).cast<String>()
          : [],
    );

  static List<ObjectWithDeprecatedFields> listFromJson(List json, {bool? growable,}) =>
    json.isNotEmpty
      ? json.map<ObjectWithDeprecatedFields>((i) => ObjectWithDeprecatedFields.fromJson(i as Map<String, dynamic>)).toList(growable: true == growable)
      : <ObjectWithDeprecatedFields>[];

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
  static Map<String, List<ObjectWithDeprecatedFields>> mapListFromJson(dynamic json, {bool? growable,}) {
    final map = <String, List<ObjectWithDeprecatedFields>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = ObjectWithDeprecatedFields.listFromJson(
            value,
            growable: growable,
          );
        });
    }
    return map;
  }
}

