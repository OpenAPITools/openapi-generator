//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
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
  // ignore: unnecessary_parenthesis
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
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static ModelFile fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();
      return ModelFile(
        sourceURI: mapValueOfType<String>(json, r'sourceURI'),
      );
    }
    return null;
  }

  static List<ModelFile> listFromJson(dynamic json, {bool emptyIsNull, bool growable,}) =>
    json is List && json.isNotEmpty
      ? json.map(ModelFile.fromJson).toList(growable: true == growable)
      : true == emptyIsNull ? null : <ModelFile>[];

  static Map<String, ModelFile> mapFromJson(dynamic json) {
    final map = <String, ModelFile>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) => map[key] = ModelFile.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of ModelFile-objects as value to a dart map
  static Map<String, List<ModelFile>> mapListFromJson(dynamic json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<ModelFile>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = ModelFile.listFromJson(
            value,
            emptyIsNull: emptyIsNull,
            growable: growable,
          );
        });
    }
    return map;
  }
}

