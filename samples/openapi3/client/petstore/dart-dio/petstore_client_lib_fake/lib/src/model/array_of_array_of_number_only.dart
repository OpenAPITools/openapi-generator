//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_collection/built_collection.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'array_of_array_of_number_only.g.dart';

/// ArrayOfArrayOfNumberOnly
///
/// Properties:
/// * [arrayArrayNumber] 
@BuiltValue()
abstract class ArrayOfArrayOfNumberOnly implements Built<ArrayOfArrayOfNumberOnly, ArrayOfArrayOfNumberOnlyBuilder> {
  @BuiltValueField(wireName: r'ArrayArrayNumber')
  BuiltList<BuiltList<num>>? get arrayArrayNumber;

  ArrayOfArrayOfNumberOnly._();

  factory ArrayOfArrayOfNumberOnly([void updates(ArrayOfArrayOfNumberOnlyBuilder b)]) = _$ArrayOfArrayOfNumberOnly;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(ArrayOfArrayOfNumberOnlyBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<ArrayOfArrayOfNumberOnly> get serializer => _$ArrayOfArrayOfNumberOnlySerializer();
}

class _$ArrayOfArrayOfNumberOnlySerializer implements PrimitiveSerializer<ArrayOfArrayOfNumberOnly> {
  @override
  final Iterable<Type> types = const [ArrayOfArrayOfNumberOnly, _$ArrayOfArrayOfNumberOnly];

  @override
  final String wireName = r'ArrayOfArrayOfNumberOnly';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    ArrayOfArrayOfNumberOnly object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.arrayArrayNumber != null) {
      yield r'ArrayArrayNumber';
      yield serializers.serialize(
        object.arrayArrayNumber,
        specifiedType: const FullType(BuiltList, [FullType(BuiltList, [FullType(num)])]),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    ArrayOfArrayOfNumberOnly object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required ArrayOfArrayOfNumberOnlyBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'ArrayArrayNumber':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(BuiltList, [FullType(BuiltList, [FullType(num)])]),
          ) as BuiltList<BuiltList<num>>;
          result.arrayArrayNumber.replace(valueDes);
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  ArrayOfArrayOfNumberOnly deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = ArrayOfArrayOfNumberOnlyBuilder();
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

