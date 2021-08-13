//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
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
  /// [json] if it's non-null, null if [json] is null.
  static ObjectWithDeprecatedFields fromJson(Map<String, dynamic> json) => json == null
    ? null
    : ObjectWithDeprecatedFields(
        uuid: json[r'uuid'],
        id: json[r'id'] == null ?
          null :
          json[r'id'].toDouble(),
        deprecatedRef: DeprecatedObject.fromJson(json[r'deprecatedRef']),
        bars: json[r'bars'] == null
          ? null
          : (json[r'bars'] as List).cast<String>(),
    );

  static List<ObjectWithDeprecatedFields> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <ObjectWithDeprecatedFields>[]
      : json.map((dynamic value) => ObjectWithDeprecatedFields.fromJson(value)).toList(growable: true == growable);

  static Map<String, ObjectWithDeprecatedFields> mapFromJson(Map<String, dynamic> json) {
    final map = <String, ObjectWithDeprecatedFields>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) => map[key] = ObjectWithDeprecatedFields.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of ObjectWithDeprecatedFields-objects as value to a dart map
  static Map<String, List<ObjectWithDeprecatedFields>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<ObjectWithDeprecatedFields>>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) {
        map[key] = ObjectWithDeprecatedFields.listFromJson(value, emptyIsNull: emptyIsNull, growable: growable,);
      });
    }
    return map;
  }
}

