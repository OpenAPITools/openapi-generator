//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class CatAllOf {
  /// Returns a new [CatAllOf] instance.
  CatAllOf({
    this.declawed,
  });

  bool declawed;

  @override
  bool operator ==(Object other) => identical(this, other) || other is CatAllOf &&
     other.declawed == declawed;

  @override
  int get hashCode =>
  // ignore: unnecessary_parenthesis
    (declawed == null ? 0 : declawed.hashCode);

  @override
  String toString() => 'CatAllOf[declawed=$declawed]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (declawed != null) {
      json[r'declawed'] = declawed;
    }
    return json;
  }

  /// Returns a new [CatAllOf] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static CatAllOf fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();
      return CatAllOf(
        declawed: mapValueOfType<bool>(json, r'declawed'),
      );
    }
    return null;
  }

  static List<CatAllOf> listFromJson(dynamic json, {bool emptyIsNull, bool growable,}) =>
    json is List && json.isNotEmpty
      ? json.map(CatAllOf.fromJson).toList(growable: true == growable)
      : true == emptyIsNull ? null : <CatAllOf>[];

  static Map<String, CatAllOf> mapFromJson(dynamic json) {
    final map = <String, CatAllOf>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) => map[key] = CatAllOf.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of CatAllOf-objects as value to a dart map
  static Map<String, List<CatAllOf>> mapListFromJson(dynamic json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<CatAllOf>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = CatAllOf.listFromJson(
            value,
            emptyIsNull: emptyIsNull,
            growable: growable,
          );
        });
    }
    return map;
  }
}

