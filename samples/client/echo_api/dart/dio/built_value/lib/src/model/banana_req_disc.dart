//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'banana_req_disc.g.dart';

/// BananaReqDisc
///
/// Properties:
/// * [length] 
/// * [fruitType] 
@BuiltValue()
abstract class BananaReqDisc implements Built<BananaReqDisc, BananaReqDiscBuilder> {
  @BuiltValueField(wireName: r'length')
  int get length;

  @BuiltValueField(wireName: r'fruitType')
  String get fruitType;

  BananaReqDisc._();

  factory BananaReqDisc([void updates(BananaReqDiscBuilder b)]) = _$BananaReqDisc;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(BananaReqDiscBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<BananaReqDisc> get serializer => _$BananaReqDiscSerializer();
}

class _$BananaReqDiscSerializer implements PrimitiveSerializer<BananaReqDisc> {
  @override
  final Iterable<Type> types = const [BananaReqDisc, _$BananaReqDisc];

  @override
  final String wireName = r'BananaReqDisc';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    BananaReqDisc object, {
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
    BananaReqDisc object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required BananaReqDiscBuilder result,
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
  BananaReqDisc deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = BananaReqDiscBuilder();
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
    
    

