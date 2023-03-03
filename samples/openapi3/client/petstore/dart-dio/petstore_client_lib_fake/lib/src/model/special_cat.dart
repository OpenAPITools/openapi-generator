//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_collection/built_collection.dart';
import 'package:openapi/src/model/cat.dart';
import 'package:openapi/src/model/special_cat_all_of.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'special_cat.g.dart';

/// SpecialCat
///
/// Properties:
/// * [className] 
/// * [color] 
/// * [declawed] 
/// * [kind] 
@BuiltValue()
abstract class SpecialCat implements Cat, SpecialCatAllOf, Built<SpecialCat, SpecialCatBuilder> {
  SpecialCat._();

  factory SpecialCat([void updates(SpecialCatBuilder b)]) = _$SpecialCat;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults(SpecialCatBuilder b) => b..className=b.discriminatorValue
      ..color = 'red';

  @BuiltValueSerializer(custom: true)
  static Serializer<SpecialCat> get serializer => _$SpecialCatSerializer();
}

class _$SpecialCatSerializer implements PrimitiveSerializer<SpecialCat> {
  @override
  final Iterable<Type> types = const [SpecialCat, _$SpecialCat];

  @override
  final String wireName = r'SpecialCat';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    SpecialCat object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
    yield r'className';
    yield serializers.serialize(
      object.className,
      specifiedType: const FullType(String),
    );
    if (object.color != null) {
      yield r'color';
      yield serializers.serialize(
        object.color,
        specifiedType: const FullType(String),
      );
    }
    if (object.declawed != null) {
      yield r'declawed';
      yield serializers.serialize(
        object.declawed,
        specifiedType: const FullType(bool),
      );
    }
    if (object.kind != null) {
      yield r'kind';
      yield serializers.serialize(
        object.kind,
        specifiedType: const FullType(SpecialCatAllOfKindEnum),
      );
    }
  }

  @override
  Object serialize(
    Serializers serializers,
    SpecialCat object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required SpecialCatBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
        case r'className':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.className = valueDes;
          break;
        case r'color':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(String),
          ) as String;
          result.color = valueDes;
          break;
        case r'declawed':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(bool),
          ) as bool;
          result.declawed = valueDes;
          break;
        case r'kind':
          final valueDes = serializers.deserialize(
            value,
            specifiedType: const FullType(SpecialCatAllOfKindEnum),
          ) as SpecialCatAllOfKindEnum;
          result.kind = valueDes;
          break;
        default:
          unhandled.add(key);
          unhandled.add(value);
          break;
      }
    }
  }

  @override
  SpecialCat deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = SpecialCatBuilder();
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

class SpecialCatKindEnum extends EnumClass {

  @BuiltValueEnumConst(wireName: r'lions')
  static const SpecialCatKindEnum lions = _$specialCatKindEnum_lions;
  @BuiltValueEnumConst(wireName: r'tigers')
  static const SpecialCatKindEnum tigers = _$specialCatKindEnum_tigers;
  @BuiltValueEnumConst(wireName: r'leopards')
  static const SpecialCatKindEnum leopards = _$specialCatKindEnum_leopards;
  @BuiltValueEnumConst(wireName: r'jaguars')
  static const SpecialCatKindEnum jaguars = _$specialCatKindEnum_jaguars;
  @BuiltValueEnumConst(wireName: r'unknown_default_open_api', fallback: true)
  static const SpecialCatKindEnum unknownDefaultOpenApi = _$specialCatKindEnum_unknownDefaultOpenApi;

  static Serializer<SpecialCatKindEnum> get serializer => _$specialCatKindEnumSerializer;

  const SpecialCatKindEnum._(String name): super(name);

  static BuiltSet<SpecialCatKindEnum> get values => _$specialCatKindEnumValues;
  static SpecialCatKindEnum valueOf(String name) => _$specialCatKindEnumValueOf(name);
}

