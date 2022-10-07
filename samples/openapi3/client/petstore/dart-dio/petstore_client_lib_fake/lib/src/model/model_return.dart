//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'model_return.g.dart';

/// Model for testing reserved words
///
/// Properties:
/// * [return_] 
@BuiltValue()
abstract class ModelReturn implements Built<ModelReturn, ModelReturnBuilder> {
  @BuiltValueField(wireName: r'return')
  int? get return_;

  ModelReturn._();

  factory ModelReturn([void updates(ModelReturnBuilder b)]) = _$ModelReturn;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(ModelReturnBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<ModelReturn> get serializer => _$ModelReturnSerializer();
}

class _$ModelReturnSerializer implements PrimitiveSerializer<ModelReturn> {
  @override
  final Iterable<Type> types = const [ModelReturn, _$ModelReturn];

  @override
  final String wireName = r'ModelReturn';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    ModelReturn object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.return_ != null) {
      yield r'return';
      yield serializers.serialize(
        object.return_,
        specifiedType: const FullType(int),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    ModelReturn object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required ModelReturnBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'return':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(int),
          ) as int;
          result.return_ = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  ModelReturn deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = ModelReturnBuilder();
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

