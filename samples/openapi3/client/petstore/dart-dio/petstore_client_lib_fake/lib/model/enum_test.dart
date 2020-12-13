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
    EnumTestEnumString get enumString;
    // enum enumStringEnum {  UPPER,  lower,  ,  };
    
    @nullable
    @BuiltValueField(wireName: r'enum_string_required')
    EnumTestEnumStringRequired get enumStringRequired;
    // enum enumStringRequiredEnum {  UPPER,  lower,  ,  };
    
    @nullable
    @BuiltValueField(wireName: r'enum_integer')
    EnumTestEnumInteger get enumInteger;
    // enum enumIntegerEnum {  1,  -1,  };
    
    @nullable
    @BuiltValueField(wireName: r'enum_number')
    EnumTestEnumNumber get enumNumber;
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

    // Boilerplate code needed to wire-up generated code
    EnumTest._();

    factory EnumTest([updates(EnumTestBuilder b)]) = _$EnumTest;
    static Serializer<EnumTest> get serializer => _$enumTestSerializer;
}

class EnumTestEnumString extends EnumClass {

  @BuiltValueEnumConst(wireName: r'UPPER')
  static const EnumTestEnumString UPPER = _$enumTestEnumString_UPPER;
  @BuiltValueEnumConst(wireName: r'lower')
  static const EnumTestEnumString lower = _$enumTestEnumString_lower;
  @BuiltValueEnumConst(wireName: r'')
  static const EnumTestEnumString empty = _$enumTestEnumString_empty;

  static Serializer<EnumTestEnumString> get serializer => _$enumTestEnumStringSerializer;

  const EnumTestEnumString._(String name): super(name);

  static BuiltSet<EnumTestEnumString> get values => _$enumTestEnumStringValues;
  static EnumTestEnumString valueOf(String name) => _$enumTestEnumStringValueOf(name);
}


class EnumTestEnumStringRequired extends EnumClass {

  @BuiltValueEnumConst(wireName: r'UPPER')
  static const EnumTestEnumStringRequired UPPER = _$enumTestEnumStringRequired_UPPER;
  @BuiltValueEnumConst(wireName: r'lower')
  static const EnumTestEnumStringRequired lower = _$enumTestEnumStringRequired_lower;
  @BuiltValueEnumConst(wireName: r'')
  static const EnumTestEnumStringRequired empty = _$enumTestEnumStringRequired_empty;

  static Serializer<EnumTestEnumStringRequired> get serializer => _$enumTestEnumStringRequiredSerializer;

  const EnumTestEnumStringRequired._(String name): super(name);

  static BuiltSet<EnumTestEnumStringRequired> get values => _$enumTestEnumStringRequiredValues;
  static EnumTestEnumStringRequired valueOf(String name) => _$enumTestEnumStringRequiredValueOf(name);
}


class EnumTestEnumInteger extends EnumClass {

  @BuiltValueEnumConst(wireNumber: 1)
  static const EnumTestEnumInteger number1 = _$enumTestEnumInteger_number1;
  @BuiltValueEnumConst(wireNumber: -1)
  static const EnumTestEnumInteger numberNegative1 = _$enumTestEnumInteger_numberNegative1;

  static Serializer<EnumTestEnumInteger> get serializer => _$enumTestEnumIntegerSerializer;

  const EnumTestEnumInteger._(String name): super(name);

  static BuiltSet<EnumTestEnumInteger> get values => _$enumTestEnumIntegerValues;
  static EnumTestEnumInteger valueOf(String name) => _$enumTestEnumIntegerValueOf(name);
}


class EnumTestEnumNumber extends EnumClass {

  @BuiltValueEnumConst(wireName: r'1.1')
  static const EnumTestEnumNumber number1Period1 = _$enumTestEnumNumber_number1Period1;
  @BuiltValueEnumConst(wireName: r'-1.2')
  static const EnumTestEnumNumber numberNegative1Period2 = _$enumTestEnumNumber_numberNegative1Period2;

  static Serializer<EnumTestEnumNumber> get serializer => _$enumTestEnumNumberSerializer;

  const EnumTestEnumNumber._(String name): super(name);

  static BuiltSet<EnumTestEnumNumber> get values => _$enumTestEnumNumberValues;
  static EnumTestEnumNumber valueOf(String name) => _$enumTestEnumNumberValueOf(name);
}


