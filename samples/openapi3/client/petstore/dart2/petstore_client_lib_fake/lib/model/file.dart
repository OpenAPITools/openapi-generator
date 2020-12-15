//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class File {
  /// Returns a new [File] instance.
  File({
    this.sourceURI,
  });

  /// Test capitalization
  String sourceURI;

  @override
  bool operator ==(Object other) => identical(this, other) || other is File &&
     other.sourceURI == sourceURI;

  @override
  int get hashCode =>
    (sourceURI == null ? 0 : sourceURI.hashCode);

  @override
  String toString() => 'File[sourceURI=$sourceURI]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (sourceURI != null) {
      json[r'sourceURI'] = sourceURI;
    }
    return json;
  }

  /// Returns a new [File] instance and imports its values from
  /// [json] if it's non-null, null if [json] is null.
  static File fromJson(Map<String, dynamic> json) => json == null
    ? null
    : File(
        sourceURI: json[r'sourceURI'],
    );

  static List<File> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <File>[]
      : json.map((v) => File.fromJson(v)).toList(growable: true == growable);

  static Map<String, File> mapFromJson(Map<String, dynamic> json) {
    final map = <String, File>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) => map[key] = File.fromJson(v));
    }
    return map;
  }

  // maps a json object with a list of File-objects as value to a dart map
  static Map<String, List<File>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<File>>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) {
        map[key] = File.listFromJson(v, emptyIsNull: emptyIsNull, growable: growable);
      });
    }
    return map;
  }
}

