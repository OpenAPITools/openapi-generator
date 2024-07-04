//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_collection/built_collection.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'array_of_number_only.g.dart';

/// ArrayOfNumberOnly
///
/// Properties:
/// * [arrayNumber] 
@BuiltValue()
abstract class ArrayOfNumberOnly implements Built<ArrayOfNumberOnly, ArrayOfNumberOnlyBuilder> {
  @BuiltValueField(wireName: r'ArrayNumber')
  BuiltList<num>? get arrayNumber;

  ArrayOfNumberOnly._();

  factory ArrayOfNumberOnly([void updates(ArrayOfNumberOnlyBuilder b)]) = _$ArrayOfNumberOnly;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(ArrayOfNumberOnlyBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<ArrayOfNumberOnly> get serializer => _$ArrayOfNumberOnlySerializer();
}

class _$ArrayOfNumberOnlySerializer implements PrimitiveSerializer<ArrayOfNumberOnly> {
  @override
  final Iterable<Type> types = const [ArrayOfNumberOnly, _$ArrayOfNumberOnly];

  @override
  final String wireName = r'ArrayOfNumberOnly';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    ArrayOfNumberOnly object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.arrayNumber != null) {
      yield r'ArrayNumber';
      yield serializers.serialize(
        object.arrayNumber,
        specifiedType: const FullType(BuiltList, [FullType(num)]),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    ArrayOfNumberOnly object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required ArrayOfNumberOnlyBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'ArrayNumber':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(BuiltList, [FullType(num)]),
          ) as BuiltList<num>;
          result.arrayNumber.replace(valueDes);
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  ArrayOfNumberOnly deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = ArrayOfNumberOnlyBuilder();
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

