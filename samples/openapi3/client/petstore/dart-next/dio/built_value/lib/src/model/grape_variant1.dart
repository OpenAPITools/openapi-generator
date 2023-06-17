//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'grape_variant1.g.dart';

/// GrapeVariant1
///
/// Properties:
/// * [color] 
@BuiltValue()
abstract class GrapeVariant1 implements Built<GrapeVariant1, GrapeVariant1Builder> {
  @BuiltValueField(wireName: r'color')
  String? get color;

  GrapeVariant1._();

  factory GrapeVariant1([void updates(GrapeVariant1Builder b)]) = _$GrapeVariant1;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(GrapeVariant1Builder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<GrapeVariant1> get serializer => _$GrapeVariant1Serializer();
}

class _$GrapeVariant1Serializer implements PrimitiveSerializer<GrapeVariant1> {
  @override
  final Iterable<Type> types = const [GrapeVariant1, _$GrapeVariant1];

  @override
  final String wireName = r'GrapeVariant1';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    GrapeVariant1 object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.color != null) {
      yield r'color';
      yield serializers.serialize(
        object.color,
        specifiedType: const FullType(String),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    GrapeVariant1 object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required GrapeVariant1Builder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'color':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.color = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  GrapeVariant1 deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = GrapeVariant1Builder();
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
    

