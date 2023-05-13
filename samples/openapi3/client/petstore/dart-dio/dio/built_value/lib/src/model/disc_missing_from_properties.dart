//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'disc_missing_from_properties.g.dart';

/// DiscMissingFromProperties
///
/// Properties:
/// * [length]
@BuiltValue()
abstract class DiscMissingFromProperties
    implements
        Built<DiscMissingFromProperties, DiscMissingFromPropertiesBuilder> {
  @BuiltValueField(wireName: r'length')
  int? get length;

  DiscMissingFromProperties._();

  factory DiscMissingFromProperties(
          [void updates(DiscMissingFromPropertiesBuilder b)]) =
      _$DiscMissingFromProperties;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(DiscMissingFromPropertiesBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<DiscMissingFromProperties> get serializer =>
      _$DiscMissingFromPropertiesSerializer();
}

class _$DiscMissingFromPropertiesSerializer
    implements PrimitiveSerializer<DiscMissingFromProperties> {
  @override
  final Iterable<Type> types = const [
    DiscMissingFromProperties,
    _$DiscMissingFromProperties
  ];

  @override
  final String wireName = r'DiscMissingFromProperties';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    DiscMissingFromProperties object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.length != null) {
      yield r'length';
      yield serializers.serialize(
        object.length,
        specifiedType: const FullType(int),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    DiscMissingFromProperties object, {
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
    required DiscMissingFromPropertiesBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'length':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(int),
          ) as int;
          result.length = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  DiscMissingFromProperties deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = DiscMissingFromPropertiesBuilder();
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
