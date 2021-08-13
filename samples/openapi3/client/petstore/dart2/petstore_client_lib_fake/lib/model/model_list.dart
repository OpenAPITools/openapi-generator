//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
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
  /// [json] if it's non-null, null if [json] is null.
  static ModelList fromJson(Map<String, dynamic> json) => json == null
    ? null
    : ModelList(
        n123list: json[r'123-list'],
    );

  static List<ModelList> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <ModelList>[]
      : json.map((dynamic value) => ModelList.fromJson(value)).toList(growable: true == growable);

  static Map<String, ModelList> mapFromJson(Map<String, dynamic> json) {
    final map = <String, ModelList>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) => map[key] = ModelList.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of ModelList-objects as value to a dart map
  static Map<String, List<ModelList>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<ModelList>>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) {
        map[key] = ModelList.listFromJson(value, emptyIsNull: emptyIsNull, growable: growable,);
      });
    }
    return map;
  }
}

