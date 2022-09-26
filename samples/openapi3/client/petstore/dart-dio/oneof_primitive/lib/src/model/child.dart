//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'child.g.dart';

/// Child
///
/// Properties:
/// * [name] 
@BuiltValue()
abstract class Child implements Built<Child, ChildBuilder> {
  @BuiltValueField(wireName: r'name')
  String? get name;

  Child._();

  factory Child([void updates(ChildBuilder b)]) = _$Child;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(ChildBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<Child> get serializer => _$ChildSerializer();
}

class _$ChildSerializer implements PrimitiveSerializer<Child> {
  @override
  final Iterable<Type> types = const [Child, _$Child];

  @override
  final String wireName = r'Child';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    Child object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.name != null) {
      yield r'name';
      yield serializers.serialize(
        object.name,
        specifiedType: const FullType(String),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    Child object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required ChildBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'name':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.name = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  Child deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = ChildBuilder();
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

