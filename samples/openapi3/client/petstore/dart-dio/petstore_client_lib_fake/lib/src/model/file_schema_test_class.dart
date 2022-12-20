//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
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
@BuiltValue()
abstract class FileSchemaTestClass implements Built<FileSchemaTestClass, FileSchemaTestClassBuilder> {
  @BuiltValueField(wireName: r'file')
  ModelFile? get file;

  @BuiltValueField(wireName: r'files')
  BuiltList<ModelFile>? get files;

  FileSchemaTestClass._();

  factory FileSchemaTestClass([void updates(FileSchemaTestClassBuilder b)]) = _$FileSchemaTestClass;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(FileSchemaTestClassBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<FileSchemaTestClass> get serializer => _$FileSchemaTestClassSerializer();
}

class _$FileSchemaTestClassSerializer implements PrimitiveSerializer<FileSchemaTestClass> {
  @override
  final Iterable<Type> types = const [FileSchemaTestClass, _$FileSchemaTestClass];

  @override
  final String wireName = r'FileSchemaTestClass';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    FileSchemaTestClass object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.file != null) {
      yield r'file';
      yield serializers.serialize(
        object.file,
        specifiedType: const FullType(ModelFile),
      );
    }
    if (object.files != null) {
      yield r'files';
      yield serializers.serialize(
        object.files,
        specifiedType: const FullType(BuiltList, [FullType(ModelFile)]),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    FileSchemaTestClass object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required FileSchemaTestClassBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'file':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(ModelFile),
          ) as ModelFile;
          result.file.replace(valueDes);
          break;
        case r'files':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(BuiltList, [FullType(ModelFile)]),
          ) as BuiltList<ModelFile>;
          result.files.replace(valueDes);
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  FileSchemaTestClass deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = FileSchemaTestClassBuilder();
    final serializedList = (serialized as Iterable<Object?>).toList();
    final unhandled = <Object?>[];
    _deserializeProperties(
      serializers,
      serialized,
      specifiedType: specifiedType,
      serializedList: serializedList,
      unhandled: unhandled,
      result: result,
    );
    return result.build();
  }
}

