//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'deprecated_object.g.dart';

/// DeprecatedObject
///
/// Properties:
/// * [name] 
@Deprecated('DeprecatedObject has been deprecated')
@BuiltValue()
abstract class DeprecatedObject implements Built<DeprecatedObject, DeprecatedObjectBuilder> {
  @BuiltValueField(wireName: r'name')
  String? get name;

  DeprecatedObject._();

  factory DeprecatedObject([void updates(DeprecatedObjectBuilder b)]) = _$DeprecatedObject;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(DeprecatedObjectBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<DeprecatedObject> get serializer => _$DeprecatedObjectSerializer();
}

class _$DeprecatedObjectSerializer implements PrimitiveSerializer<DeprecatedObject> {
  @override
  final Iterable<Type> types = const [DeprecatedObject, _$DeprecatedObject];

  @override
  final String wireName = r'DeprecatedObject';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    DeprecatedObject object, {
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
    DeprecatedObject object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required DeprecatedObjectBuilder result,
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
  DeprecatedObject deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = DeprecatedObjectBuilder();
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

