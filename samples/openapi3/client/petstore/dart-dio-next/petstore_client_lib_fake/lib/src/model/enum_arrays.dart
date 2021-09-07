//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//

import 'package:built_collection/built_collection.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'enum_arrays.g.dart';

/// EnumArrays
///
/// Properties:
/// * [justSymbol] 
/// * [arrayEnum] 
abstract class EnumArrays implements Built<EnumArrays, EnumArraysBuilder> {
    @BuiltValueField(wireName: r'just_symbol')
    EnumArraysJustSymbolEnum? get justSymbol;
    // enum justSymbolEnum {  >=,  $,  };

    @BuiltValueField(wireName: r'array_enum')
    BuiltList<EnumArraysArrayEnumEnum>? get arrayEnum;
    // enum arrayEnumEnum {  fish,  crab,  };

    EnumArrays._();

    @BuiltValueHook(initializeBuilder: true)
    static void _defaults(EnumArraysBuilder b) => b;

    factory EnumArrays([void updates(EnumArraysBuilder b)]) = _$EnumArrays;

    @BuiltValueSerializer(custom: true)
    static Serializer<EnumArrays> get serializer => _$EnumArraysSerializer();
}

class _$EnumArraysSerializer implements StructuredSerializer<EnumArrays> {
    @override
    final Iterable<Type> types = const [EnumArrays, _$EnumArrays];

    @override
    final String wireName = r'EnumArrays';

    @override
    Iterable<Object?> serialize(Serializers serializers, EnumArrays object,
        {FullType specifiedType = FullType.unspecified}) {
        final result = <Object?>[];
        if (object.justSymbol != null) {
            result
                ..add(r'just_symbol')
                ..add(serializers.serialize(object.justSymbol,
                    specifiedType: const FullType(EnumArraysJustSymbolEnum)));
        }
        if (object.arrayEnum != null) {
            result
                ..add(r'array_enum')
                ..add(serializers.serialize(object.arrayEnum,
                    specifiedType: const FullType(BuiltList, [FullType(EnumArraysArrayEnumEnum)])));
        }
        return result;
    }

    @override
    EnumArrays deserialize(Serializers serializers, Iterable<Object?> serialized,
        {FullType specifiedType = FullType.unspecified}) {
        final result = EnumArraysBuilder();

        final iterator = serialized.iterator;
        while (iterator.moveNext()) {
            final key = iterator.current as String;
            iterator.moveNext();
            final Object? value = iterator.current;
            
            switch (key) {
                case r'just_symbol':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(EnumArraysJustSymbolEnum)) as EnumArraysJustSymbolEnum;
                    result.justSymbol = valueDes;
                    break;
                case r'array_enum':
                    final valueDes = serializers.deserialize(value,
                        specifiedType: const FullType(BuiltList, [FullType(EnumArraysArrayEnumEnum)])) as BuiltList<EnumArraysArrayEnumEnum>;
                    result.arrayEnum.replace(valueDes);
                    break;
            }
        }
        return result.build();
    }
}

class EnumArraysJustSymbolEnum extends EnumClass {

  @BuiltValueEnumConst(wireName: r'>=')
  static const EnumArraysJustSymbolEnum greaterThanEqual = _$enumArraysJustSymbolEnum_greaterThanEqual;
  @BuiltValueEnumConst(wireName: r'$')
  static const EnumArraysJustSymbolEnum dollar = _$enumArraysJustSymbolEnum_dollar;

  static Serializer<EnumArraysJustSymbolEnum> get serializer => _$enumArraysJustSymbolEnumSerializer;

  const EnumArraysJustSymbolEnum._(String name): super(name);

  static BuiltSet<EnumArraysJustSymbolEnum> get values => _$enumArraysJustSymbolEnumValues;
  static EnumArraysJustSymbolEnum valueOf(String name) => _$enumArraysJustSymbolEnumValueOf(name);
}

class EnumArraysArrayEnumEnum extends EnumClass {

  @BuiltValueEnumConst(wireName: r'fish')
  static const EnumArraysArrayEnumEnum fish = _$enumArraysArrayEnumEnum_fish;
  @BuiltValueEnumConst(wireName: r'crab')
  static const EnumArraysArrayEnumEnum crab = _$enumArraysArrayEnumEnum_crab;

  static Serializer<EnumArraysArrayEnumEnum> get serializer => _$enumArraysArrayEnumEnumSerializer;

  const EnumArraysArrayEnumEnum._(String name): super(name);

  static BuiltSet<EnumArraysArrayEnumEnum> get values => _$enumArraysArrayEnumEnumValues;
  static EnumArraysArrayEnumEnum valueOf(String name) => _$enumArraysArrayEnumEnumValueOf(name);
}

