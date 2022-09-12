//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'has_only_read_only.g.dart';

/// HasOnlyReadOnly
///
/// Properties:
/// * [bar] 
/// * [foo] 
@BuiltValue()
abstract class HasOnlyReadOnly implements Built<HasOnlyReadOnly, HasOnlyReadOnlyBuilder> {
  @BuiltValueField(wireName: r'bar')
  String? get bar;

  @BuiltValueField(wireName: r'foo')
  String? get foo;

  HasOnlyReadOnly._();

  factory HasOnlyReadOnly([void updates(HasOnlyReadOnlyBuilder b)]) = _$HasOnlyReadOnly;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(HasOnlyReadOnlyBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<HasOnlyReadOnly> get serializer => _$HasOnlyReadOnlySerializer();
}

class _$HasOnlyReadOnlySerializer implements PrimitiveSerializer<HasOnlyReadOnly> {
  @override
  final Iterable<Type> types = const [HasOnlyReadOnly, _$HasOnlyReadOnly];

  @override
  final String wireName = r'HasOnlyReadOnly';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    HasOnlyReadOnly object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.bar != null) {
      yield r'bar';
      yield serializers.serialize(
        object.bar,
        specifiedType: const FullType(String),
      );
    }
    if (object.foo != null) {
      yield r'foo';
      yield serializers.serialize(
        object.foo,
        specifiedType: const FullType(String),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    HasOnlyReadOnly object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required HasOnlyReadOnlyBuilder result,
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
        case r'foo':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.foo = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  HasOnlyReadOnly deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = HasOnlyReadOnlyBuilder();
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

