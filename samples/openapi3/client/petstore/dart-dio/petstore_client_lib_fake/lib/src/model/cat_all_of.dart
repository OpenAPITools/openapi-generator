//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'cat_all_of.g.dart';

/// CatAllOf
///
/// Properties:
/// * [declawed] 
@BuiltValue(instantiable: false)
abstract class CatAllOf  {
  @BuiltValueField(wireName: r'declawed')
  bool? get declawed;

  @BuiltValueSerializer(custom: true)
  static Serializer<CatAllOf> get serializer => _$CatAllOfSerializer();
}

class _$CatAllOfSerializer implements PrimitiveSerializer<CatAllOf> {
  @override
  final Iterable<Type> types = const [CatAllOf];

  @override
  final String wireName = r'CatAllOf';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    CatAllOf object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    if (object.declawed != null) {
      yield r'declawed';
      yield serializers.serialize(
        object.declawed,
        specifiedType: const FullType(bool),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    CatAllOf object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  @override
  CatAllOf deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return serializers.deserialize(serialized, specifiedType: FullType($CatAllOf)) as $CatAllOf;
  }
}

/// a concrete implementation of [CatAllOf], since [CatAllOf] is not instantiable
@BuiltValue(instantiable: true)
abstract class $CatAllOf implements CatAllOf, Built<$CatAllOf, $CatAllOfBuilder> {
  $CatAllOf._();

  factory $CatAllOf([void Function($CatAllOfBuilder)? updates]) = _$$CatAllOf;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults($CatAllOfBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<$CatAllOf> get serializer => _$$CatAllOfSerializer();
}

class _$$CatAllOfSerializer implements PrimitiveSerializer<$CatAllOf> {
  @override
  final Iterable<Type> types = const [$CatAllOf, _$$CatAllOf];

  @override
  final String wireName = r'$CatAllOf';

  @override
  Object serialize(
    Serializers serializers,
    $CatAllOf object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return serializers.serialize(object, specifiedType: FullType(CatAllOf))!;
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required CatAllOfBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'declawed':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(bool),
          ) as bool;
          result.declawed = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  $CatAllOf deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = $CatAllOfBuilder();
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

