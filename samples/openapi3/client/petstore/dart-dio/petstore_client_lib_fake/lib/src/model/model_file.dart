//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'model_file.g.dart';

/// Must be named `File` for test.
///
/// Properties:
/// * [sourceURI] - Test capitalization
@BuiltValue()
abstract class ModelFile implements Built<ModelFile, ModelFileBuilder> {
  /// Test capitalization
  @BuiltValueField(wireName: r'sourceURI')
  String? get sourceURI;

  ModelFile._();

  factory ModelFile([void updates(ModelFileBuilder b)]) = _$ModelFile;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(ModelFileBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<ModelFile> get serializer => _$ModelFileSerializer();
}

class _$ModelFileSerializer implements PrimitiveSerializer<ModelFile> {
  @override
  final Iterable<Type> types = const [ModelFile, _$ModelFile];

  @override
  final String wireName = r'ModelFile';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    ModelFile object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.sourceURI != null) {
      yield r'sourceURI';
      yield serializers.serialize(
        object.sourceURI,
        specifiedType: const FullType(String),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    ModelFile object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required ModelFileBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'sourceURI':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.sourceURI = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  ModelFile deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = ModelFileBuilder();
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

