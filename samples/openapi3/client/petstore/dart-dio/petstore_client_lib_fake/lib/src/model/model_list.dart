//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'model_list.g.dart';

/// ModelList
///
/// Properties:
/// * [n123list] 
@BuiltValue()
abstract class ModelList implements Built<ModelList, ModelListBuilder> {
  @BuiltValueField(wireName: r'123-list')
  String? get n123list;

  ModelList._();

  factory ModelList([void updates(ModelListBuilder b)]) = _$ModelList;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(ModelListBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<ModelList> get serializer => _$ModelListSerializer();
}

class _$ModelListSerializer implements PrimitiveSerializer<ModelList> {
  @override
  final Iterable<Type> types = const [ModelList, _$ModelList];

  @override
  final String wireName = r'ModelList';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    ModelList object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.n123list != null) {
      yield r'123-list';
      yield serializers.serialize(
        object.n123list,
        specifiedType: const FullType(String),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    ModelList object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required ModelListBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'123-list':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.n123list = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  ModelList deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = ModelListBuilder();
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

