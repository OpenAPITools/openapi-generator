//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.6

// ignore_for_file: unused_import

import 'package:openapi/model/model_file.dart';
import 'package:built_collection/built_collection.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'file_schema_test_class.g.dart';

abstract class FileSchemaTestClass implements Built<FileSchemaTestClass, FileSchemaTestClassBuilder> {

    @nullable
    @BuiltValueField(wireName: r'file')
    ModelFile get file;

    @nullable
    @BuiltValueField(wireName: r'files')
    BuiltList<ModelFile> get files;

    // Boilerplate code needed to wire-up generated code
    FileSchemaTestClass._();

    static void _initializeBuilder(FileSchemaTestClassBuilder b) => b;

    factory FileSchemaTestClass([void updates(FileSchemaTestClassBuilder b)]) = _$FileSchemaTestClass;
    static Serializer<FileSchemaTestClass> get serializer => _$fileSchemaTestClassSerializer;
}

