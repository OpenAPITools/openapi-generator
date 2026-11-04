//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/model_file.dart';
import 'package:freezed_annotation/freezed_annotation.dart';

part 'file_schema_test_class.freezed.dart';
part 'file_schema_test_class.g.dart';


@freezed
abstract class FileSchemaTestClass with _$FileSchemaTestClass {
  const factory FileSchemaTestClass({
    @JsonKey(
      name: r'file',
      required: false,
      includeIfNull: false,
    )
    ModelFile? file,
    @JsonKey(
      name: r'files',
      required: false,
      includeIfNull: false,
    )
    List<ModelFile>? files,
  }) = _FileSchemaTestClass;

  factory FileSchemaTestClass.fromJson(Map<String, dynamic> json) =>
      _$FileSchemaTestClassFromJson(json);
}

