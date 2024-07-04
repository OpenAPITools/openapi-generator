//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'foo.g.dart';

/// Foo
///
/// Properties:
/// * [bar] 
@BuiltValue()
abstract class Foo implements Built<Foo, FooBuilder> {
  @BuiltValueField(wireName: r'bar')
  String? get bar;

  Foo._();

  factory Foo([void updates(FooBuilder b)]) = _$Foo;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(FooBuilder b) => b
      ..bar = 'bar';

  @BuiltValueSerializer(custom: true)
  static Serializer<Foo> get serializer => _$FooSerializer();
}

class _$FooSerializer implements PrimitiveSerializer<Foo> {
  @override
  final Iterable<Type> types = const [Foo, _$Foo];

  @override
  final String wireName = r'Foo';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    Foo object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.bar != null) {
      yield r'bar';
      yield serializers.serialize(
        object.bar,
        specifiedType: const FullType(String),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    Foo object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required FooBuilder result,
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
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  Foo deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = FooBuilder();
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

