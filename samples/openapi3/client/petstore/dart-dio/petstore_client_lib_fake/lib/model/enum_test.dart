//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.7

// ignore_for_file: unused_import

import 'package:openapi/model/outer_enum_integer_default_value.dart';
import 'package:built_collection/built_collection.dart';
import 'package:openapi/model/outer_enum_default_value.dart';
import 'package:openapi/model/outer_enum.dart';
import 'package:openapi/model/outer_enum_integer.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'enum_test.g.dart';

abstract class EnumTest implements Built<EnumTest, EnumTestBuilder> {

    @nullable
    @BuiltValueField(wireName: r'enum_string')
    EnumTestEnumStringEnum get enumString;
    // enum enumStringEnum {  UPPER,  lower,  ,  };

    @BuiltValueField(wireName: r'enum_string_required')
    EnumTestEnumStringRequiredEnum get enumStringRequired;
    // enum enumStringRequiredEnum {  UPPER,  lower,  ,  };

    @nullable
    @BuiltValueField(wireName: r'enum_integer')
    EnumTestEnumIntegerEnum get enumInteger;
    // enum enumIntegerEnum {  1,  -1,  };

    @nullable
    @BuiltValueField(wireName: r'enum_number')
    EnumTestEnumNumberEnum get enumNumber;
    // enum enumNumberEnum {  1.1,  -1.2,  };

    @nullable
    @BuiltValueField(wireName: r'outerEnum')
    OuterEnum get outerEnum;
    // enum outerEnumEnum {  placed,  approved,  delivered,  };

    @nullable
    @BuiltValueField(wireName: r'outerEnumInteger')
    OuterEnumInteger get outerEnumInteger;
    // enum outerEnumIntegerEnum {  0,  1,  2,  };

    @nullable
    @BuiltValueField(wireName: r'outerEnumDefaultValue')
    OuterEnumDefaultValue get outerEnumDefaultValue;
    // enum outerEnumDefaultValueEnum {  placed,  approved,  delivered,  };

    @nullable
    @BuiltValueField(wireName: r'outerEnumIntegerDefaultValue')
    OuterEnumIntegerDefaultValue get outerEnumIntegerDefaultValue;
    // enum outerEnumIntegerDefaultValueEnum {  0,  1,  2,  };

    EnumTest._();

    static void _initializeBuilder(EnumTestBuilder b) => b;

    factory EnumTest([void updates(EnumTestBuilder b)]) = _$EnumTest;

    @BuiltValueSerializer(custom: true)
    static Serializer<EnumTest> get serializer => _$EnumTestSerializer();
}

class _$EnumTestSerializer implements StructuredSerializer<EnumTest> {

    @override
    final Iterable<Type> types = const [EnumTest, _$EnumTest];
    @override
    final String wireName = r'EnumTest';

    @override
    Iterable<Object> serialize(Serializers serializers, EnumTest object,
        {FullType specifiedType = FullType.unspecified}) {
        final result = <Object>[];
        if (object.enumString != null) {
            result
                ..add(r'enum_string')
                ..add(serializers.serialize(object.enumString,
                    specifiedType: const FullType(EnumTestEnumStringEnum)));
        }
        result
            ..add(r'enum_string_required')
            ..add(serializers.serialize(object.enumStringRequired,
                specifiedType: const FullType(EnumTestEnumStringRequiredEnum)));
        if (object.enumInteger != null) {
            result
                ..add(r'enum_integer')
                ..add(serializers.serialize(object.enumInteger,
                    specifiedType: const FullType(EnumTestEnumIntegerEnum)));
        }
        if (object.enumNumber != null) {
            result
                ..add(r'enum_number')
                ..add(serializers.serialize(object.enumNumber,
                    specifiedType: const FullType(EnumTestEnumNumberEnum)));
        }
        if (object.outerEnum != null) {
            result
                ..add(r'outerEnum')
                ..add(serializers.serialize(object.outerEnum,
                    specifiedType: const FullType(OuterEnum)));
        }
        if (object.outerEnumInteger != null) {
            result
                ..add(r'outerEnumInteger')
                ..add(serializers.serialize(object.outerEnumInteger,
                    specifiedType: const FullType(OuterEnumInteger)));
        }
        if (object.outerEnumDefaultValue != null) {
            result
                ..add(r'outerEnumDefaultValue')
                ..add(serializers.serialize(object.outerEnumDefaultValue,
                    specifiedType: const FullType(OuterEnumDefaultValue)));
        }
        if (object.outerEnumIntegerDefaultValue != null) {
            result
                ..add(r'outerEnumIntegerDefaultValue')
                ..add(serializers.serialize(object.outerEnumIntegerDefaultValue,
                    specifiedType: const FullType(OuterEnumIntegerDefaultValue)));
        }
        return result;
    }

    @override
    EnumTest deserialize(Serializers serializers, Iterable<Object> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = EnumTestBuilder();

        final iterator = serialized.iterator;
        while (iterator.moveNext()) {
            final key = iterator.current as String;
            iterator.moveNext();
            final dynamic value = iterator.current;
            switch (key) {
                case r'enum_string':
                    result.enumString = serializers.deserialize(value,
                        specifiedType: const FullType(EnumTestEnumStringEnum)) as EnumTestEnumStringEnum;
                    break;
                case r'enum_string_required':
                    result.enumStringRequired = serializers.deserialize(value,
                        specifiedType: const FullType(EnumTestEnumStringRequiredEnum)) as EnumTestEnumStringRequiredEnum;
                    break;
                case r'enum_integer':
                    result.enumInteger = serializers.deserialize(value,
                        specifiedType: const FullType(EnumTestEnumIntegerEnum)) as EnumTestEnumIntegerEnum;
                    break;
                case r'enum_number':
                    result.enumNumber = serializers.deserialize(value,
                        specifiedType: const FullType(EnumTestEnumNumberEnum)) as EnumTestEnumNumberEnum;
                    break;
                case r'outerEnum':
                    result.outerEnum = serializers.deserialize(value,
                        specifiedType: const FullType(OuterEnum)) as OuterEnum;
                    break;
                case r'outerEnumInteger':
                    result.outerEnumInteger = serializers.deserialize(value,
                        specifiedType: const FullType(OuterEnumInteger)) as OuterEnumInteger;
                    break;
                case r'outerEnumDefaultValue':
                    result.outerEnumDefaultValue = serializers.deserialize(value,
                        specifiedType: const FullType(OuterEnumDefaultValue)) as OuterEnumDefaultValue;
                    break;
                case r'outerEnumIntegerDefaultValue':
                    result.outerEnumIntegerDefaultValue = serializers.deserialize(value,
                        specifiedType: const FullType(OuterEnumIntegerDefaultValue)) as OuterEnumIntegerDefaultValue;
                    break;
            }
        }
        return result.build();
    }
}

class EnumTestEnumStringEnum extends EnumClass {

  @BuiltValueEnumConst(wireName: r'UPPER')
  static const EnumTestEnumStringEnum UPPER = _$enumTestEnumStringEnum_UPPER;
  @BuiltValueEnumConst(wireName: r'lower')
  static const EnumTestEnumStringEnum lower = _$enumTestEnumStringEnum_lower;
  @BuiltValueEnumConst(wireName: r'')
  static const EnumTestEnumStringEnum empty = _$enumTestEnumStringEnum_empty;

  static Serializer<EnumTestEnumStringEnum> get serializer => _$enumTestEnumStringEnumSerializer;

  const EnumTestEnumStringEnum._(String name): super(name);

  static BuiltSet<EnumTestEnumStringEnum> get values => _$enumTestEnumStringEnumValues;
  static EnumTestEnumStringEnum valueOf(String name) => _$enumTestEnumStringEnumValueOf(name);
}

class EnumTestEnumStringRequiredEnum extends EnumClass {

  @BuiltValueEnumConst(wireName: r'UPPER')
  static const EnumTestEnumStringRequiredEnum UPPER = _$enumTestEnumStringRequiredEnum_UPPER;
  @BuiltValueEnumConst(wireName: r'lower')
  static const EnumTestEnumStringRequiredEnum lower = _$enumTestEnumStringRequiredEnum_lower;
  @BuiltValueEnumConst(wireName: r'')
  static const EnumTestEnumStringRequiredEnum empty = _$enumTestEnumStringRequiredEnum_empty;

  static Serializer<EnumTestEnumStringRequiredEnum> get serializer => _$enumTestEnumStringRequiredEnumSerializer;

  const EnumTestEnumStringRequiredEnum._(String name): super(name);

  static BuiltSet<EnumTestEnumStringRequiredEnum> get values => _$enumTestEnumStringRequiredEnumValues;
  static EnumTestEnumStringRequiredEnum valueOf(String name) => _$enumTestEnumStringRequiredEnumValueOf(name);
}

class EnumTestEnumIntegerEnum extends EnumClass {

  @BuiltValueEnumConst(wireNumber: 1)
  static const EnumTestEnumIntegerEnum number1 = _$enumTestEnumIntegerEnum_number1;
  @BuiltValueEnumConst(wireNumber: -1)
  static const EnumTestEnumIntegerEnum numberNegative1 = _$enumTestEnumIntegerEnum_numberNegative1;

  static Serializer<EnumTestEnumIntegerEnum> get serializer => _$enumTestEnumIntegerEnumSerializer;

  const EnumTestEnumIntegerEnum._(String name): super(name);

  static BuiltSet<EnumTestEnumIntegerEnum> get values => _$enumTestEnumIntegerEnumValues;
  static EnumTestEnumIntegerEnum valueOf(String name) => _$enumTestEnumIntegerEnumValueOf(name);
}

class EnumTestEnumNumberEnum extends EnumClass {

  @BuiltValueEnumConst(wireName: r'1.1')
  static const EnumTestEnumNumberEnum number1Period1 = _$enumTestEnumNumberEnum_number1Period1;
  @BuiltValueEnumConst(wireName: r'-1.2')
  static const EnumTestEnumNumberEnum numberNegative1Period2 = _$enumTestEnumNumberEnum_numberNegative1Period2;

  static Serializer<EnumTestEnumNumberEnum> get serializer => _$enumTestEnumNumberEnumSerializer;

  const EnumTestEnumNumberEnum._(String name): super(name);

  static BuiltSet<EnumTestEnumNumberEnum> get values => _$enumTestEnumNumberEnumValues;
  static EnumTestEnumNumberEnum valueOf(String name) => _$enumTestEnumNumberEnumValueOf(name);
}

