//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/model_file.dart';
import 'package:json_annotation/json_annotation.dart';

part 'file_schema_test_class.g.dart';


@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: false,
  explicitToJson: true,
)
class FileSchemaTestClass {
  /// Returns a new [FileSchemaTestClass] instance.
  FileSchemaTestClass({

     this.file,

     this.files,
  });

  @JsonKey(
    
    name: r'file',
    required: false,
    includeIfNull: false
  )


  final ModelFile? file;



  @JsonKey(
    
    name: r'files',
    required: false,
    includeIfNull: false
  )


  final List<ModelFile>? files;



  @override
  bool operator ==(Object other) => identical(this, other) || other is FileSchemaTestClass &&
     other.file == file &&
     other.files == files;

  @override
  int get hashCode =>
    file.hashCode +
    files.hashCode;

  factory FileSchemaTestClass.fromJson(Map<String, dynamic> json) => _$FileSchemaTestClassFromJson(json);

  Map<String, dynamic> toJson() => _$FileSchemaTestClassToJson(this);

  @override
  String toString() {
    return toJson().toString();
  }

}

