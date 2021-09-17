//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'package:built_collection/built_collection.dart';
import 'package:openapi/src/model/model_file.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'file_schema_test_class.g.dart';

/// FileSchemaTestClass
///
/// Properties:
/// * [file] 
/// * [files] 
abstract class FileSchemaTestClass implements Built<FileSchemaTestClass, FileSchemaTestClassBuilder> {
    @BuiltValueField(wireName: r'file')
    ModelFile? get file;

    @BuiltValueField(wireName: r'files')
    BuiltList<ModelFile>? get files;

    FileSchemaTestClass._();

    @BuiltValueHook(initializeBuilder: true)
    static void _defaults(FileSchemaTestClassBuilder b) => b;

    factory FileSchemaTestClass([void updates(FileSchemaTestClassBuilder b)]) = _$FileSchemaTestClass;

    @BuiltValueSerializer(custom: true)
    static Serializer<FileSchemaTestClass> get serializer => _$FileSchemaTestClassSerializer();
}

class _$FileSchemaTestClassSerializer implements StructuredSerializer<FileSchemaTestClass> {
    @override
    final Iterable<Type> types = const [FileSchemaTestClass, _$FileSchemaTestClass];

    @override
    final String wireName = r'FileSchemaTestClass';

    @override
    Iterable<Object?> serialize(Serializers serializers, FileSchemaTestClass object,
        {FullType specifiedType = FullType.unspecified}) {
        final result = <Object?>[];
        if (object.file != null) {
            result
                ..add(r'file')
                ..add(serializers.serialize(object.file,
                    specifiedType: const FullType(ModelFile)));
        }
        if (object.files != null) {
            result
                ..add(r'files')
                ..add(serializers.serialize(object.files,
                    specifiedType: const FullType(BuiltList, [FullType(ModelFile)])));
        }
        return result;
    }

    @override
    FileSchemaTestClass deserialize(Serializers serializers, Iterable<Object?> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = FileSchemaTestClassBuilder();

        final iterator = serialized.iterator;
        while (iterator.moveNext()) {
            final key = iterator.current as String;
            iterator.moveNext();
            final Object? value = iterator.current;
            
            switch (key) {
                case r'file':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(ModelFile)) as ModelFile;
                    result.file.replace(valueDes);
                    break;
                case r'files':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(BuiltList, [FullType(ModelFile)])) as BuiltList<ModelFile>;
                    result.files.replace(valueDes);
                    break;
            }
        }
        return result.build();
    }
}

