//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'disc_optional_type_correct.g.dart';

/// DiscOptionalTypeCorrect
///
/// Properties:
/// * [fruitType]
@BuiltValue()
abstract class DiscOptionalTypeCorrect
    implements Built<DiscOptionalTypeCorrect, DiscOptionalTypeCorrectBuilder> {
  @BuiltValueField(wireName: r'fruitType')
  String? get fruitType;

  DiscOptionalTypeCorrect._();

  factory DiscOptionalTypeCorrect(
          [void updates(DiscOptionalTypeCorrectBuilder b)]) =
      _$DiscOptionalTypeCorrect;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(DiscOptionalTypeCorrectBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<DiscOptionalTypeCorrect> get serializer =>
      _$DiscOptionalTypeCorrectSerializer();
}

class _$DiscOptionalTypeCorrectSerializer
    implements PrimitiveSerializer<DiscOptionalTypeCorrect> {
  @override
  final Iterable<Type> types = const [
    DiscOptionalTypeCorrect,
    _$DiscOptionalTypeCorrect
  ];

  @override
  final String wireName = r'DiscOptionalTypeCorrect';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    DiscOptionalTypeCorrect object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.fruitType != null) {
      yield r'fruitType';
      yield serializers.serialize(
        object.fruitType,
        specifiedType: const FullType(String),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    DiscOptionalTypeCorrect object, {
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
    required DiscOptionalTypeCorrectBuilder result,
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
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  DiscOptionalTypeCorrect deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = DiscOptionalTypeCorrectBuilder();
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
