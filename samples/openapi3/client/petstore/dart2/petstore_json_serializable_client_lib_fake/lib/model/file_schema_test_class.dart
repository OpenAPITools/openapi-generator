//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: true,
  explicitToJson: true,
)
class FileSchemaTestClass {
  /// Returns a new [FileSchemaTestClass] instance.
  FileSchemaTestClass({
    this.file,
    this.files = const [],
  });

  @JsonKey(
    nullable: false,
    name: r'file',
    required: false,
  )
  ModelFile file;

  @JsonKey(
    defaultValue: const [],
    name: r'files',
    required: false,
  )
  List<ModelFile> files;

  @override
  bool operator ==(Object other) => identical(this, other) || other is FileSchemaTestClass &&
     other.file == file &&
     other.files == files;

  @override
  int get hashCode =>
    (file == null ? 0 : file.hashCode) +
    (files == null ? 0 : files.hashCode);

  factory FileSchemaTestClass.fromJson(Map<String, dynamic> json) => _$FileSchemaTestClassFromJson(json);

  Map<String, dynamic> toJson() => _$FileSchemaTestClassToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

