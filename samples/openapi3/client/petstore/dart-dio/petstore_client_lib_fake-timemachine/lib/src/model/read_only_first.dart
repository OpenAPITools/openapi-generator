//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'read_only_first.g.dart';

/// ReadOnlyFirst
///
/// Properties:
/// * [bar] 
/// * [baz] 
@BuiltValue()
abstract class ReadOnlyFirst implements Built<ReadOnlyFirst, ReadOnlyFirstBuilder> {
  @BuiltValueField(wireName: r'bar')
  String? get bar;

  @BuiltValueField(wireName: r'baz')
  String? get baz;

  ReadOnlyFirst._();

  factory ReadOnlyFirst([void updates(ReadOnlyFirstBuilder b)]) = _$ReadOnlyFirst;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(ReadOnlyFirstBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<ReadOnlyFirst> get serializer => _$ReadOnlyFirstSerializer();
}

class _$ReadOnlyFirstSerializer implements PrimitiveSerializer<ReadOnlyFirst> {
  @override
  final Iterable<Type> types = const [ReadOnlyFirst, _$ReadOnlyFirst];

  @override
  final String wireName = r'ReadOnlyFirst';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    ReadOnlyFirst object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.bar != null) {
      yield r'bar';
      yield serializers.serialize(
        object.bar,
        specifiedType: const FullType(String),
      );
    }
    if (object.baz != null) {
      yield r'baz';
      yield serializers.serialize(
        object.baz,
        specifiedType: const FullType(String),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    ReadOnlyFirst object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required ReadOnlyFirstBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'bar':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.bar = valueDes;
          break;
        case r'baz':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.baz = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  ReadOnlyFirst deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = ReadOnlyFirstBuilder();
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

