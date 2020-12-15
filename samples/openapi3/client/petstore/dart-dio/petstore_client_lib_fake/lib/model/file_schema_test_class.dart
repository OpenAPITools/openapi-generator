import 'package:built_collection/built_collection.dart';
import 'package:openapi/model/multipart_file.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'file_schema_test_class.g.dart';

abstract class FileSchemaTestClass implements Built<FileSchemaTestClass, FileSchemaTestClassBuilder> {

    @nullable
    @BuiltValueField(wireName: r'file')
    MultipartFile get file;

    @nullable
    @BuiltValueField(wireName: r'files')
    BuiltList<MultipartFile> get files;

    // Boilerplate code needed to wire-up generated code
    FileSchemaTestClass._();

    factory FileSchemaTestClass([updates(FileSchemaTestClassBuilder b)]) = _$FileSchemaTestClass;
    static Serializer<FileSchemaTestClass> get serializer => _$fileSchemaTestClassSerializer;
}

