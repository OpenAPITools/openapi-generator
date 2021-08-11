//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class ModelFile {
  /// Returns a new [ModelFile] instance.
  ModelFile({
    this.sourceURI,
  });

  /// Test capitalization
  String sourceURI;

  @override
  bool operator ==(Object other) => identical(this, other) || other is ModelFile &&
     other.sourceURI == sourceURI;

  @override
  int get hashCode =>
    (sourceURI == null ? 0 : sourceURI.hashCode);

  @override
  String toString() => 'ModelFile[sourceURI=$sourceURI]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (sourceURI != null) {
      json[r'sourceURI'] = sourceURI;
    }
    return json;
  }

  /// Returns a new [ModelFile] instance and imports its values from
  /// [json] if it's non-null, null if [json] is null.
  static ModelFile fromJson(Map<String, dynamic> json) => json == null
    ? null
    : ModelFile(
        sourceURI: json[r'sourceURI'],
    );

  static List<ModelFile> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <ModelFile>[]
      : json.map((v) => ModelFile.fromJson(v)).toList(growable: true == growable);

  static Map<String, ModelFile> mapFromJson(Map<String, dynamic> json) {
    final map = <String, ModelFile>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) => map[key] = ModelFile.fromJson(v));
    }
    return map;
  }

  // maps a json object with a list of ModelFile-objects as value to a dart map
  static Map<String, List<ModelFile>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<ModelFile>>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) {
        map[key] = ModelFile.listFromJson(v, emptyIsNull: emptyIsNull, growable: growable);
      });
    }
    return map;
  }
}

