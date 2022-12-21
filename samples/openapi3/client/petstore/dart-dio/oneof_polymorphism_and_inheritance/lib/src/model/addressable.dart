//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'addressable.g.dart';

/// Base schema for addressable entities
///
/// Properties:
/// * [href] - Hyperlink reference
/// * [id] - unique identifier
@BuiltValue(instantiable: false)
abstract class Addressable  {
  /// Hyperlink reference
  @BuiltValueField(wireName: r'href')
  String? get href;

  /// unique identifier
  @BuiltValueField(wireName: r'id')
  String? get id;

  @BuiltValueSerializer(custom: true)
  static Serializer<Addressable> get serializer => _$AddressableSerializer();
}

class _$AddressableSerializer implements PrimitiveSerializer<Addressable> {
  @override
  final Iterable<Type> types = const [Addressable];

  @override
  final String wireName = r'Addressable';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    Addressable object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.href != null) {
      yield r'href';
      yield serializers.serialize(
        object.href,
        specifiedType: const FullType(String),
      );
    }
    if (object.id != null) {
      yield r'id';
      yield serializers.serialize(
        object.id,
        specifiedType: const FullType(String),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    Addressable object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  @override
  Addressable deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return serializers.deserialize(serialized, specifiedType: FullType($Addressable)) as $Addressable;
  }
}

/// a concrete implementation of [Addressable], since [Addressable] is not instantiable
@BuiltValue(instantiable: true)
abstract class $Addressable implements Addressable, Built<$Addressable, $AddressableBuilder> {
  $Addressable._();

  factory $Addressable([void Function($AddressableBuilder)? updates]) = _$$Addressable;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults($AddressableBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<$Addressable> get serializer => _$$AddressableSerializer();
}

class _$$AddressableSerializer implements PrimitiveSerializer<$Addressable> {
  @override
  final Iterable<Type> types = const [$Addressable, _$$Addressable];

  @override
  final String wireName = r'$Addressable';

  @override
  Object serialize(
    Serializers serializers,
    $Addressable object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return serializers.serialize(object, specifiedType: FullType(Addressable))!;
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required AddressableBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'href':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.href = valueDes;
          break;
        case r'id':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.id = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  $Addressable deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = $AddressableBuilder();
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

