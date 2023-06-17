//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'disc_type_incorrect.g.dart';

/// DiscTypeIncorrect
///
/// Properties:
/// * [fruitType] 
@BuiltValue()
abstract class DiscTypeIncorrect implements Built<DiscTypeIncorrect, DiscTypeIncorrectBuilder> {
  @BuiltValueField(wireName: r'fruitType')
  int get fruitType;

  DiscTypeIncorrect._();

  factory DiscTypeIncorrect([void updates(DiscTypeIncorrectBuilder b)]) = _$DiscTypeIncorrect;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(DiscTypeIncorrectBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<DiscTypeIncorrect> get serializer => _$DiscTypeIncorrectSerializer();
}

class _$DiscTypeIncorrectSerializer implements PrimitiveSerializer<DiscTypeIncorrect> {
  @override
  final Iterable<Type> types = const [DiscTypeIncorrect, _$DiscTypeIncorrect];

  @override
  final String wireName = r'DiscTypeIncorrect';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    DiscTypeIncorrect object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    yield r'fruitType';
    yield serializers.serialize(
      object.fruitType,
      specifiedType: const FullType(int),
    );
  }

  @override
  Object serialize(
    Serializers serializers,
    DiscTypeIncorrect object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required DiscTypeIncorrectBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'fruitType':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(int),
          ) as int;
          result.fruitType = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  DiscTypeIncorrect deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = DiscTypeIncorrectBuilder();
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
    

