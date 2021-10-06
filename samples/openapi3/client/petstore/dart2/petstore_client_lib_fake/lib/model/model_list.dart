//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.14

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


  String? n123list;

  @override
  bool operator ==(Object other) => identical(this, other) || other is ModelList &&
     other.n123list == n123list;

  @override
  int get hashCode =>
    n123list.hashCode;

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
  static ModelList fromJson(Map<String, dynamic> json) => ModelList(
        n123list: json[r'123-list'] as String,
    );

  static List<ModelList> listFromJson(List json, {bool? growable,}) =>
    json.isNotEmpty
      ? json.map<ModelList>((i) => ModelList.fromJson(i as Map<String, dynamic>)).toList(growable: true == growable)
      : <ModelList>[];

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
  static Map<String, List<ModelList>> mapListFromJson(dynamic json, {bool? growable,}) {
    final map = <String, List<ModelList>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = ModelList.listFromJson(
            value,
            growable: growable,
          );
        });
    }
    return map;
  }
}

