//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

// ignore_for_file: unused_element
import 'package:built_collection/built_collection.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'special_cat_all_of.g.dart';

/// SpecialCatAllOf
///
/// Properties:
/// * [kind] 
@BuiltValue(instantiable: false)
abstract class SpecialCatAllOf  {
  @BuiltValueField(wireName: r'kind')
  SpecialCatAllOfKindEnum? get kind;
  // enum kindEnum {  lions,  tigers,  leopards,  jaguars,  };

  @BuiltValueSerializer(custom: true)
  static Serializer<SpecialCatAllOf> get serializer => _$SpecialCatAllOfSerializer();
}

class _$SpecialCatAllOfSerializer implements PrimitiveSerializer<SpecialCatAllOf> {
  @override
  final Iterable<Type> types = const [SpecialCatAllOf];

  @override
  final String wireName = r'SpecialCatAllOf';

  Iterable<Object?> _serializeProperties(
    Serializers serializers,
    SpecialCatAllOf object, {
    FullType specifiedType = FullType.unspecified,
  }) sync* {
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
    SpecialCatAllOf object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return _serializeProperties(serializers, object, specifiedType: specifiedType).toList();
  }

  @override
  SpecialCatAllOf deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return serializers.deserialize(serialized, specifiedType: FullType($SpecialCatAllOf)) as $SpecialCatAllOf;
  }
}

/// a concrete implementation of [SpecialCatAllOf], since [SpecialCatAllOf] is not instantiable
@BuiltValue(instantiable: true)
abstract class $SpecialCatAllOf implements SpecialCatAllOf, Built<$SpecialCatAllOf, $SpecialCatAllOfBuilder> {
  $SpecialCatAllOf._();

  factory $SpecialCatAllOf([void Function($SpecialCatAllOfBuilder)? updates]) = _$$SpecialCatAllOf;

  @BuiltValueHook(initializeBuilder: true)
  static void _defaults($SpecialCatAllOfBuilder b) => b;

  @BuiltValueSerializer(custom: true)
  static Serializer<$SpecialCatAllOf> get serializer => _$$SpecialCatAllOfSerializer();
}

class _$$SpecialCatAllOfSerializer implements PrimitiveSerializer<$SpecialCatAllOf> {
  @override
  final Iterable<Type> types = const [$SpecialCatAllOf, _$$SpecialCatAllOf];

  @override
  final String wireName = r'$SpecialCatAllOf';

  @override
  Object serialize(
    Serializers serializers,
    $SpecialCatAllOf object, {
    FullType specifiedType = FullType.unspecified,
  }) {
    return serializers.serialize(object, specifiedType: FullType(SpecialCatAllOf))!;
  }

  void _deserializeProperties(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
    required List<Object?> serializedList,
    required SpecialCatAllOfBuilder result,
    required List<Object?> unhandled,
  }) {
    for (var i = 0; i < serializedList.length; i += 2) {
      final key = serializedList[i] as String;
      final value = serializedList[i + 1];
      switch (key) {
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
  $SpecialCatAllOf deserialize(
    Serializers serializers,
    Object serialized, {
    FullType specifiedType = FullType.unspecified,
  }) {
    final result = $SpecialCatAllOfBuilder();
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

class SpecialCatAllOfKindEnum extends EnumClass {

  @BuiltValueEnumConst(wireName: r'lions')
  static const SpecialCatAllOfKindEnum lions = _$specialCatAllOfKindEnum_lions;
  @BuiltValueEnumConst(wireName: r'tigers')
  static const SpecialCatAllOfKindEnum tigers = _$specialCatAllOfKindEnum_tigers;
  @BuiltValueEnumConst(wireName: r'leopards')
  static const SpecialCatAllOfKindEnum leopards = _$specialCatAllOfKindEnum_leopards;
  @BuiltValueEnumConst(wireName: r'jaguars')
  static const SpecialCatAllOfKindEnum jaguars = _$specialCatAllOfKindEnum_jaguars;
  @BuiltValueEnumConst(wireName: r'unknown_default_open_api', fallback: true)
  static const SpecialCatAllOfKindEnum unknownDefaultOpenApi = _$specialCatAllOfKindEnum_unknownDefaultOpenApi;

  static Serializer<SpecialCatAllOfKindEnum> get serializer => _$specialCatAllOfKindEnumSerializer;

  const SpecialCatAllOfKindEnum._(String name): super(name);

  static BuiltSet<SpecialCatAllOfKindEnum> get values => _$specialCatAllOfKindEnumValues;
  static SpecialCatAllOfKindEnum valueOf(String name) => _$specialCatAllOfKindEnumValueOf(name);
}

