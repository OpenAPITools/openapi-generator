//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'orange.g.dart';

/// Orange
///
/// Properties:
/// * [sweet] 
@BuiltValue()
abstract class Orange implements Built<Orange, OrangeBuilder> {
  @BuiltValueField(wireName: r'sweet')
  bool? get sweet;

  Orange._();

  factory Orange([void updates(OrangeBuilder b)]) = _$Orange;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(OrangeBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<Orange> get serializer => _$OrangeSerializer();
}

class _$OrangeSerializer implements PrimitiveSerializer<Orange> {
  @override
  final Iterable<Type> types = const [Orange, _$Orange];

  @override
  final String wireName = r'Orange';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    Orange object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.sweet != null) {
      yield r'sweet';
      yield serializers.serialize(
        object.sweet,
        specifiedType: const FullType(bool),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    Orange object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required OrangeBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'sweet':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(bool),
          ) as bool;
          result.sweet = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  Orange deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = OrangeBuilder();
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

