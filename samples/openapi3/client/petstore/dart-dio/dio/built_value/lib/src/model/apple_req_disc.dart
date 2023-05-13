//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'apple_req_disc.g.dart';

/// AppleReqDisc
///
/// Properties:
/// * [seeds]
/// * [fruitType]
@BuiltValue()
abstract class AppleReqDisc
    implements Built<AppleReqDisc, AppleReqDiscBuilder> {
  @BuiltValueField(wireName: r'seeds')
  int get seeds;

  @BuiltValueField(wireName: r'fruitType')
  String get fruitType;

  AppleReqDisc._();

  factory AppleReqDisc([void updates(AppleReqDiscBuilder b)]) = _$AppleReqDisc;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(AppleReqDiscBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<AppleReqDisc> get serializer => _$AppleReqDiscSerializer();
}

class _$AppleReqDiscSerializer implements PrimitiveSerializer<AppleReqDisc> {
  @override
  final Iterable<Type> types = const [AppleReqDisc, _$AppleReqDisc];

  @override
  final String wireName = r'AppleReqDisc';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    AppleReqDisc object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    yield r'seeds';
    yield serializers.serialize(
      object.seeds,
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
    AppleReqDisc object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object,
            specifiedType: specifiedType)
        .toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required AppleReqDiscBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'seeds':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(int),
          ) as int;
          result.seeds = valueDes;
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
  AppleReqDisc deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = AppleReqDiscBuilder();
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
