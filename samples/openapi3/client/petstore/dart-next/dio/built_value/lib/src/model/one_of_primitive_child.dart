//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'one_of_primitive_child.g.dart';

/// OneOfPrimitiveChild
///
/// Properties:
/// * [name] 
@BuiltValue()
abstract class OneOfPrimitiveChild implements Built<OneOfPrimitiveChild, OneOfPrimitiveChildBuilder> {
  @BuiltValueField(wireName: r'name')
  String? get name;

  OneOfPrimitiveChild._();

  factory OneOfPrimitiveChild([void updates(OneOfPrimitiveChildBuilder b)]) = _$OneOfPrimitiveChild;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(OneOfPrimitiveChildBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<OneOfPrimitiveChild> get serializer => _$OneOfPrimitiveChildSerializer();
}

class _$OneOfPrimitiveChildSerializer implements PrimitiveSerializer<OneOfPrimitiveChild> {
  @override
  final Iterable<Type> types = const [OneOfPrimitiveChild, _$OneOfPrimitiveChild];

  @override
  final String wireName = r'OneOfPrimitiveChild';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    OneOfPrimitiveChild object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.name != null) {
      yield r'name';
      yield serializers.serialize(
        object.name,
        specifiedType: const FullType(String),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    OneOfPrimitiveChild object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required OneOfPrimitiveChildBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'name':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.name = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  OneOfPrimitiveChild deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = OneOfPrimitiveChildBuilder();
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
    

