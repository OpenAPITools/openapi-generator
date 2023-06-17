//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/parent.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'banana_grandparent_disc.g.dart';

/// BananaGrandparentDisc
///
/// Properties:
/// * [length] 
/// * [fruitType] 
@BuiltValue()
abstract class BananaGrandparentDisc implements Parent, Built<BananaGrandparentDisc, BananaGrandparentDiscBuilder> {
  @BuiltValueField(wireName: r'length')
  int get length;

  BananaGrandparentDisc._();

  factory BananaGrandparentDisc([void updates(BananaGrandparentDiscBuilder b)]) = _$BananaGrandparentDisc;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(BananaGrandparentDiscBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<BananaGrandparentDisc> get serializer => _$BananaGrandparentDiscSerializer();
}

class _$BananaGrandparentDiscSerializer implements PrimitiveSerializer<BananaGrandparentDisc> {
  @override
  final Iterable<Type> types = const [BananaGrandparentDisc, _$BananaGrandparentDisc];

  @override
  final String wireName = r'BananaGrandparentDisc';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    BananaGrandparentDisc object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    yield r'length';
    yield serializers.serialize(
      object.length,
      specifiedType: const FullType(int),
    );
    yield r'fruitType';
    yield serializers.serialize(
      object.fruitType,
      specifiedType: const FullType(String),
    );
  }

  @override
  Object serialize(
    Serializers serializers,
    BananaGrandparentDisc object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required BananaGrandparentDiscBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'length':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(int),
          ) as int;
          result.length = valueDes;
          break;
        case r'fruitType':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
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
  BananaGrandparentDisc deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = BananaGrandparentDiscBuilder();
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
    
    

