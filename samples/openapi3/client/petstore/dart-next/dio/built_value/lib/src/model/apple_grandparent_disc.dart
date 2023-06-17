//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:openapi/src/model/parent.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'apple_grandparent_disc.g.dart';

/// AppleGrandparentDisc
///
/// Properties:
/// * [seeds] 
/// * [fruitType] 
@BuiltValue()
abstract class AppleGrandparentDisc implements Parent, Built<AppleGrandparentDisc, AppleGrandparentDiscBuilder> {
  @BuiltValueField(wireName: r'seeds')
  int get seeds;

  AppleGrandparentDisc._();

  factory AppleGrandparentDisc([void updates(AppleGrandparentDiscBuilder b)]) = _$AppleGrandparentDisc;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(AppleGrandparentDiscBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<AppleGrandparentDisc> get serializer => _$AppleGrandparentDiscSerializer();
}

class _$AppleGrandparentDiscSerializer implements PrimitiveSerializer<AppleGrandparentDisc> {
  @override
  final Iterable<Type> types = const [AppleGrandparentDisc, _$AppleGrandparentDisc];

  @override
  final String wireName = r'AppleGrandparentDisc';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    AppleGrandparentDisc object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    yield r'fruitType';
    yield serializers.serialize(
      object.fruitType,
      specifiedType: const FullType(String),
    );
    yield r'seeds';
    yield serializers.serialize(
      object.seeds,
      specifiedType: const FullType(int),
    );
  }

  @override
  Object serialize(
    Serializers serializers,
    AppleGrandparentDisc object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required AppleGrandparentDiscBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'fruitType':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.fruitType = valueDes;
          break;
        case r'seeds':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(int),
          ) as int;
          result.seeds = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  AppleGrandparentDisc deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = AppleGrandparentDiscBuilder();
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
    
    

