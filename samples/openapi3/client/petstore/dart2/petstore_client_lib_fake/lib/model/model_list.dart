//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class ModelList {
  /// Returns a new [ModelList] instance.
  ModelList({
    this.n123list,
  });

  String n123list;

  @override
  bool operator ==(Object other) => identical(this, other) || other is ModelList &&
     other.n123list == n123list;

  @override
  int get hashCode =>
  // ignore: unnecessary_parenthesis
    (n123list == null ? 0 : n123list.hashCode);

  @override
  String toString() => 'ModelList[n123list=$n123list]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (n123list != null) {
      json[r'123-list'] = n123list;
    }
    return json;
  }

  /// Returns a new [ModelList] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static ModelList fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();
      return ModelList(
        n123list: mapValueOfType<String>(json, r'123-list'),
      );
    }
    return null;
  }

  static List<ModelList> listFromJson(dynamic json, {bool emptyIsNull, bool growable,}) =>
    json is List && json.isNotEmpty
      ? json.map(ModelList.fromJson).toList(growable: true == growable)
      : true == emptyIsNull ? null : <ModelList>[];

  static Map<String, ModelList> mapFromJson(dynamic json) {
    final map = <String, ModelList>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) => map[key] = ModelList.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of ModelList-objects as value to a dart map
  static Map<String, List<ModelList>> mapListFromJson(dynamic json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<ModelList>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = ModelList.listFromJson(
            value,
            emptyIsNull: emptyIsNull,
            growable: growable,
          );
        });
    }
    return map;
  }
}

