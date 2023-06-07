//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'number_only.g.dart';

/// NumberOnly
///
/// Properties:
/// * [justNumber] 
@BuiltValue()
abstract class NumberOnly implements Built<NumberOnly, NumberOnlyBuilder> {
  @BuiltValueField(wireName: r'JustNumber')
  num? get justNumber;

  NumberOnly._();

  factory NumberOnly([void updates(NumberOnlyBuilder b)]) = _$NumberOnly;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(NumberOnlyBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<NumberOnly> get serializer => _$NumberOnlySerializer();
}

class _$NumberOnlySerializer implements PrimitiveSerializer<NumberOnly> {
  @override
  final Iterable<Type> types = const [NumberOnly, _$NumberOnly];

  @override
  final String wireName = r'NumberOnly';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    NumberOnly object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.justNumber != null) {
      yield r'JustNumber';
      yield serializers.serialize(
        object.justNumber,
        specifiedType: const FullType(num),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    NumberOnly object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required NumberOnlyBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'JustNumber':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(num),
          ) as num;
          result.justNumber = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  NumberOnly deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = NumberOnlyBuilder();
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

