//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class FileSchemaTestClass {
  /// Returns a new [FileSchemaTestClass] instance.
  FileSchemaTestClass({
    this.file,
    this.files = const [],
  });

  ModelFile file;

  List<ModelFile> files;

  @override
  bool operator ==(Object other) => identical(this, other) || other is FileSchemaTestClass &&
     other.file == file &&
     other.files == files;

  @override
  int get hashCode =>
  // ignore: unnecessary_parenthesis
    (file == null ? 0 : file.hashCode) +
    (files == null ? 0 : files.hashCode);

  @override
  String toString() => 'FileSchemaTestClass[file=$file, files=$files]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (file != null) {
      json[r'file'] = file;
    }
    if (files != null) {
      json[r'files'] = files;
    }
    return json;
  }

  /// Returns a new [FileSchemaTestClass] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static FileSchemaTestClass fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();
      return FileSchemaTestClass(
        file: ModelFile.fromJson(json[r'file']),
        files: ModelFile.listFromJson(json[r'files']),
      );
    }
    return null;
  }

  static List<FileSchemaTestClass> listFromJson(dynamic json, {bool emptyIsNull, bool growable,}) =>
    json is List && json.isNotEmpty
      ? json.map(FileSchemaTestClass.fromJson).toList(growable: true == growable)
      : true == emptyIsNull ? null : <FileSchemaTestClass>[];

  static Map<String, FileSchemaTestClass> mapFromJson(dynamic json) {
    final map = <String, FileSchemaTestClass>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) => map[key] = FileSchemaTestClass.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of FileSchemaTestClass-objects as value to a dart map
  static Map<String, List<FileSchemaTestClass>> mapListFromJson(dynamic json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<FileSchemaTestClass>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = FileSchemaTestClass.listFromJson(
            value,
            emptyIsNull: emptyIsNull,
            growable: growable,
          );
        });
    }
    return map;
  }
}

