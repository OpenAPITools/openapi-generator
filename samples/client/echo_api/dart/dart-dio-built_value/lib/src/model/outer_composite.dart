//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'outer_composite.g.dart';

/// OuterComposite
///
/// Properties:
/// * [myNumber] 
/// * [myString] 
/// * [myBoolean] 
@BuiltValue()
abstract class OuterComposite implements Built<OuterComposite, OuterCompositeBuilder> {
  @BuiltValueField(wireName: r'my_number')
  num? get myNumber;

  @BuiltValueField(wireName: r'my_string')
  String? get myString;

  @BuiltValueField(wireName: r'my_boolean')
  bool? get myBoolean;

  OuterComposite._();

  factory OuterComposite([void updates(OuterCompositeBuilder b)]) = _$OuterComposite;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(OuterCompositeBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<OuterComposite> get serializer => _$OuterCompositeSerializer();
}

class _$OuterCompositeSerializer implements PrimitiveSerializer<OuterComposite> {
  @override
  final Iterable<Type> types = const [OuterComposite, _$OuterComposite];

  @override
  final String wireName = r'OuterComposite';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    OuterComposite object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.myNumber != null) {
      yield r'my_number';
      yield serializers.serialize(
        object.myNumber,
        specifiedType: const FullType(num),
      );
    }
    if (object.myString != null) {
      yield r'my_string';
      yield serializers.serialize(
        object.myString,
        specifiedType: const FullType(String),
      );
    }
    if (object.myBoolean != null) {
      yield r'my_boolean';
      yield serializers.serialize(
        object.myBoolean,
        specifiedType: const FullType(bool),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    OuterComposite object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required OuterCompositeBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'my_number':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(num),
          ) as num;
          result.myNumber = valueDes;
          break;
        case r'my_string':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.myString = valueDes;
          break;
        case r'my_boolean':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(bool),
          ) as bool;
          result.myBoolean = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  OuterComposite deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = OuterCompositeBuilder();
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

