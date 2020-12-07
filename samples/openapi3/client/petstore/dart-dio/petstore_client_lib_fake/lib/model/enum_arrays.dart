import 'package:built_collection/built_collection.dart';
import 'package:built_value/built_value.dart';
import 'package:built_value/serializer.dart';

part 'enum_arrays.g.dart';

abstract class EnumArrays implements Built<EnumArrays, EnumArraysBuilder> {

    
    @nullable
    @BuiltValueField(wireName: r'just_symbol')
    EnumArraysJustSymbol get justSymbol;
    // enum justSymbolEnum {  &gt;&#x3D;,  $,  };
    
    @nullable
    @BuiltValueField(wireName: r'array_enum')
    EnumArraysArrayEnum get arrayEnum;
    // enum arrayEnumEnum {  fish,  crab,  };

    // Boilerplate code needed to wire-up generated code
    EnumArrays._();

    factory EnumArrays([updates(EnumArraysBuilder b)]) = _$EnumArrays;
    static Serializer<EnumArrays> get serializer => _$enumArraysSerializer;
}

class EnumArraysJustSymbol extends EnumClass {

  @BuiltValueEnumConst(wireName: '>=')
  static const EnumArraysJustSymbol &gt;&#x3D; = _$enumArraysJustSymbol_&gt;&#x3D;;
  @BuiltValueEnumConst(wireName: '$')
  static const EnumArraysJustSymbol $ = _$enumArraysJustSymbol_$;

  static Serializer<EnumArraysJustSymbol> get serializer => _$enumArraysJustSymbolSerializer;

  const EnumArraysJustSymbol._(String name): super(name);

  static BuiltSet<EnumArraysJustSymbol> get values => _$enumArraysJustSymbolValues;
  static EnumArraysJustSymbol valueOf(String name) => _$enumArraysJustSymbolValueOf(name);
}


class EnumArraysArrayEnum extends EnumClass {

  @BuiltValueEnumConst(wireName: 'fish')
  static const EnumArraysArrayEnum fish = _$enumArraysArrayEnum_fish;
  @BuiltValueEnumConst(wireName: 'crab')
  static const EnumArraysArrayEnum crab = _$enumArraysArrayEnum_crab;

  static Serializer<EnumArraysArrayEnum> get serializer => _$enumArraysArrayEnumSerializer;

  const EnumArraysArrayEnum._(String name): super(name);

  static BuiltSet<EnumArraysArrayEnum> get values => _$enumArraysArrayEnumValues;
  static EnumArraysArrayEnum valueOf(String name) => _$enumArraysArrayEnumValueOf(name);
}


