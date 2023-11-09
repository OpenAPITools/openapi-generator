//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'apple_variant1.g.dart';

/// AppleVariant1
///
/// Properties:
/// * [kind] 
@BuiltValue()
abstract class AppleVariant1 implements Built<AppleVariant1, AppleVariant1Builder> {
  @BuiltValueField(wireName: r'kind')
  String? get kind;

  AppleVariant1._();

  factory AppleVariant1([void updates(AppleVariant1Builder b)]) = _$AppleVariant1;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(AppleVariant1Builder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<AppleVariant1> get serializer => _$AppleVariant1Serializer();
}

class _$AppleVariant1Serializer implements PrimitiveSerializer<AppleVariant1> {
  @override
  final Iterable<Type> types = const [AppleVariant1, _$AppleVariant1];

  @override
  final String wireName = r'AppleVariant1';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    AppleVariant1 object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.kind != null) {
      yield r'kind';
      yield serializers.serialize(
        object.kind,
        specifiedType: const FullType(String),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    AppleVariant1 object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required AppleVariant1Builder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'kind':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.kind = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  AppleVariant1 deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = AppleVariant1Builder();
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
    

