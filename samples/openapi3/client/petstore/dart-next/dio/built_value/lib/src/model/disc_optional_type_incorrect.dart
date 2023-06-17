//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'disc_optional_type_incorrect.g.dart';

/// DiscOptionalTypeIncorrect
///
/// Properties:
/// * [fruitType]
@BuiltValue()
abstract class DiscOptionalTypeIncorrect
    implements
        Built<DiscOptionalTypeIncorrect, DiscOptionalTypeIncorrectBuilder> {
  @BuiltValueField(wireName: r'fruitType')
  int? get fruitType;

  DiscOptionalTypeIncorrect._();

  factory DiscOptionalTypeIncorrect(
          [void updates(DiscOptionalTypeIncorrectBuilder b)]) =
      _$DiscOptionalTypeIncorrect;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(DiscOptionalTypeIncorrectBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<DiscOptionalTypeIncorrect> get serializer =>
      _$DiscOptionalTypeIncorrectSerializer();
}

class _$DiscOptionalTypeIncorrectSerializer
    implements PrimitiveSerializer<DiscOptionalTypeIncorrect> {
  @override
  final Iterable<Type> types = const [
    DiscOptionalTypeIncorrect,
    _$DiscOptionalTypeIncorrect
  ];

  @override
  final String wireName = r'DiscOptionalTypeIncorrect';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    DiscOptionalTypeIncorrect object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.fruitType != null) {
      yield r'fruitType';
      yield serializers.serialize(
        object.fruitType,
        specifiedType: const FullType(int),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    DiscOptionalTypeIncorrect object, {
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
    required DiscOptionalTypeIncorrectBuilder result,
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
  DiscOptionalTypeIncorrect deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = DiscOptionalTypeIncorrectBuilder();
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
