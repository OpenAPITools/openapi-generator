// Model def

import 'package:petstore_api/_internal.dart';


part 'enum_test.reflection.dart';


/// EnumTestMixin
///
/// Properties:
/// * [enumString] 
/// * [enumStringRequired] 
/// * [enumInteger] 
/// * [enumIntegerOnly] 
/// * [enumNumber] 
/// * [outerEnum] 
/// * [outerEnumInteger] 
/// * [outerEnumDefaultValue] 
/// * [outerEnumIntegerDefaultValue] 
mixin EnumTestMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            EnumTestEnumStringEnum
> get enumString;

            EnumTestEnumStringRequiredEnum
 get enumStringRequired;
UndefinedWrapper<
            EnumTestEnumIntegerEnum
> get enumInteger;
UndefinedWrapper<
            EnumTestEnumIntegerOnlyEnum
> get enumIntegerOnly;
UndefinedWrapper<
            EnumTestEnumNumberEnum
> get enumNumber;
UndefinedWrapper<
            OuterEnum
?> get outerEnum;
UndefinedWrapper<
            OuterEnumInteger
> get outerEnumInteger;
UndefinedWrapper<
            OuterEnumDefaultValue
> get outerEnumDefaultValue;
UndefinedWrapper<
            OuterEnumIntegerDefaultValue
> get outerEnumIntegerDefaultValue;
  
}

/// EnumTest
///
/// Properties:
/// * [enumString] 
/// * [enumStringRequired] 
/// * [enumInteger] 
/// * [enumIntegerOnly] 
/// * [enumNumber] 
/// * [outerEnum] 
/// * [outerEnumInteger] 
/// * [outerEnumDefaultValue] 
/// * [outerEnumIntegerDefaultValue] 
class EnumTest with
$OpenApiObjectMixin,

EnumTestMixin {
  @override
  UndefinedWrapper<
            EnumTestEnumStringEnum
> enumString;
  @override
  
            EnumTestEnumStringRequiredEnum
 enumStringRequired;
  @override
  UndefinedWrapper<
            EnumTestEnumIntegerEnum
> enumInteger;
  @override
  UndefinedWrapper<
            EnumTestEnumIntegerOnlyEnum
> enumIntegerOnly;
  @override
  UndefinedWrapper<
            EnumTestEnumNumberEnum
> enumNumber;
  @override
  UndefinedWrapper<
            OuterEnum
?> outerEnum;
  @override
  UndefinedWrapper<
            OuterEnumInteger
> outerEnumInteger;
  @override
  UndefinedWrapper<
            OuterEnumDefaultValue
> outerEnumDefaultValue;
  @override
  UndefinedWrapper<
            OuterEnumIntegerDefaultValue
> outerEnumIntegerDefaultValue;

  AdditionalProperties<Object
?> additionalProperties;

  

  EnumTest.$all({
        required this.enumString,
    required this.enumStringRequired,
    required this.enumInteger,
    required this.enumIntegerOnly,
    required this.enumNumber,
    required this.outerEnum,
    required this.outerEnumInteger,
    required this.outerEnumDefaultValue,
    required this.outerEnumIntegerDefaultValue,
    required this.additionalProperties,
    
  });

  EnumTest({
      this.enumString = const UndefinedWrapper
        .undefined()
,
required  this.enumStringRequired     ,
  this.enumInteger = const UndefinedWrapper
        .undefined()
,
  this.enumIntegerOnly = const UndefinedWrapper
        .undefined()
,
  this.enumNumber = const UndefinedWrapper
        .undefined()
,
  this.outerEnum = const UndefinedWrapper
        .undefined()
,
  this.outerEnumInteger = const UndefinedWrapper
        .undefined()
,
  this.outerEnumDefaultValue = const UndefinedWrapper
        .undefined()
,
  this.outerEnumIntegerDefaultValue = const UndefinedWrapper
        .undefined()
,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = EnumTestReflection.instance;
  EnumTestReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory EnumTest.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  EnumTest clone() {
    return $reflection.clone(this);
  }
}


extension type const EnumTestEnumStringEnum._(String value) implements String {
      const EnumTestEnumStringEnum.UPPER() : this._(r'UPPER');
      const EnumTestEnumStringEnum.lower() : this._(r'lower');
      const EnumTestEnumStringEnum.empty() : this._(r'');

  /// Creates a [EnumTestEnumStringEnum] enum from a value and safely checking if it exists.
  factory EnumTestEnumStringEnum.$safe(String value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  static const $reflection = EnumReflection<EnumTestEnumStringEnum, String>(
    PrimitiveReflection.forString,
    members: [
      
        EnumMemberReflection(dartName: r'UPPER', oasValue: r'UPPER', value: EnumTestEnumStringEnum.UPPER()),
      
        EnumMemberReflection(dartName: r'lower', oasValue: r'lower', value: EnumTestEnumStringEnum.lower()),
      
        EnumMemberReflection(dartName: r'empty', oasValue: r'', value: EnumTestEnumStringEnum.empty()),
      
    ],
  );

  factory EnumTestEnumStringEnum.deserialize(Object? value, [SerializationContext context = const SerializationContext.json()]) {
    return $reflection.deserializeFunction(value, context);
  }

  static bool canDeserialize(Object? value, [SerializationContext context = const SerializationContext.json()]) {
    return $reflection.canDeserializeFunction(value,context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json()]) {
    return $reflection.serializeFunction(this, context);
  }

  /// Creates a [EnumTestEnumStringEnum] enum from a value without checking if it exists.
  const EnumTestEnumStringEnum.$unsafe(String value) : this._(value);

  /// All possible values of the enum.
  static const List<EnumTestEnumStringEnum> values = [
    EnumTestEnumStringEnum.UPPER(),
    EnumTestEnumStringEnum.lower(),
    EnumTestEnumStringEnum.empty(),
    
  ];
}


extension type const EnumTestEnumStringRequiredEnum._(String value) implements String {
      const EnumTestEnumStringRequiredEnum.UPPER() : this._(r'UPPER');
      const EnumTestEnumStringRequiredEnum.lower() : this._(r'lower');
      const EnumTestEnumStringRequiredEnum.empty() : this._(r'');

  /// Creates a [EnumTestEnumStringRequiredEnum] enum from a value and safely checking if it exists.
  factory EnumTestEnumStringRequiredEnum.$safe(String value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  static const $reflection = EnumReflection<EnumTestEnumStringRequiredEnum, String>(
    PrimitiveReflection.forString,
    members: [
      
        EnumMemberReflection(dartName: r'UPPER', oasValue: r'UPPER', value: EnumTestEnumStringRequiredEnum.UPPER()),
      
        EnumMemberReflection(dartName: r'lower', oasValue: r'lower', value: EnumTestEnumStringRequiredEnum.lower()),
      
        EnumMemberReflection(dartName: r'empty', oasValue: r'', value: EnumTestEnumStringRequiredEnum.empty()),
      
    ],
  );

  factory EnumTestEnumStringRequiredEnum.deserialize(Object? value, [SerializationContext context = const SerializationContext.json()]) {
    return $reflection.deserializeFunction(value, context);
  }

  static bool canDeserialize(Object? value, [SerializationContext context = const SerializationContext.json()]) {
    return $reflection.canDeserializeFunction(value,context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json()]) {
    return $reflection.serializeFunction(this, context);
  }

  /// Creates a [EnumTestEnumStringRequiredEnum] enum from a value without checking if it exists.
  const EnumTestEnumStringRequiredEnum.$unsafe(String value) : this._(value);

  /// All possible values of the enum.
  static const List<EnumTestEnumStringRequiredEnum> values = [
    EnumTestEnumStringRequiredEnum.UPPER(),
    EnumTestEnumStringRequiredEnum.lower(),
    EnumTestEnumStringRequiredEnum.empty(),
    
  ];
}


extension type const EnumTestEnumIntegerEnum._(int value) implements int {
      const EnumTestEnumIntegerEnum.number1() : this._(1);
      const EnumTestEnumIntegerEnum.numberNegative1() : this._(-1);

  /// Creates a [EnumTestEnumIntegerEnum] enum from a value and safely checking if it exists.
  factory EnumTestEnumIntegerEnum.$safe(int value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  static const $reflection = EnumReflection<EnumTestEnumIntegerEnum, int>(
    PrimitiveReflection.forint,
    members: [
      
        EnumMemberReflection(dartName: r'number1', oasValue: 1, value: EnumTestEnumIntegerEnum.number1()),
      
        EnumMemberReflection(dartName: r'numberNegative1', oasValue: -1, value: EnumTestEnumIntegerEnum.numberNegative1()),
      
    ],
  );

  factory EnumTestEnumIntegerEnum.deserialize(Object? value, [SerializationContext context = const SerializationContext.json()]) {
    return $reflection.deserializeFunction(value, context);
  }

  static bool canDeserialize(Object? value, [SerializationContext context = const SerializationContext.json()]) {
    return $reflection.canDeserializeFunction(value,context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json()]) {
    return $reflection.serializeFunction(this, context);
  }

  /// Creates a [EnumTestEnumIntegerEnum] enum from a value without checking if it exists.
  const EnumTestEnumIntegerEnum.$unsafe(int value) : this._(value);

  /// All possible values of the enum.
  static const List<EnumTestEnumIntegerEnum> values = [
    EnumTestEnumIntegerEnum.number1(),
    EnumTestEnumIntegerEnum.numberNegative1(),
    
  ];
}


extension type const EnumTestEnumIntegerOnlyEnum._(int value) implements int {
      const EnumTestEnumIntegerOnlyEnum.number2() : this._(2);
      const EnumTestEnumIntegerOnlyEnum.numberNegative2() : this._(-2);

  /// Creates a [EnumTestEnumIntegerOnlyEnum] enum from a value and safely checking if it exists.
  factory EnumTestEnumIntegerOnlyEnum.$safe(int value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  static const $reflection = EnumReflection<EnumTestEnumIntegerOnlyEnum, int>(
    PrimitiveReflection.forint,
    members: [
      
        EnumMemberReflection(dartName: r'number2', oasValue: 2, value: EnumTestEnumIntegerOnlyEnum.number2()),
      
        EnumMemberReflection(dartName: r'numberNegative2', oasValue: -2, value: EnumTestEnumIntegerOnlyEnum.numberNegative2()),
      
    ],
  );

  factory EnumTestEnumIntegerOnlyEnum.deserialize(Object? value, [SerializationContext context = const SerializationContext.json()]) {
    return $reflection.deserializeFunction(value, context);
  }

  static bool canDeserialize(Object? value, [SerializationContext context = const SerializationContext.json()]) {
    return $reflection.canDeserializeFunction(value,context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json()]) {
    return $reflection.serializeFunction(this, context);
  }

  /// Creates a [EnumTestEnumIntegerOnlyEnum] enum from a value without checking if it exists.
  const EnumTestEnumIntegerOnlyEnum.$unsafe(int value) : this._(value);

  /// All possible values of the enum.
  static const List<EnumTestEnumIntegerOnlyEnum> values = [
    EnumTestEnumIntegerOnlyEnum.number2(),
    EnumTestEnumIntegerOnlyEnum.numberNegative2(),
    
  ];
}


extension type const EnumTestEnumNumberEnum._(double value) implements double {
      const EnumTestEnumNumberEnum.number11() : this._(1.1);
      const EnumTestEnumNumberEnum.numberNegative12() : this._(-1.2);

  /// Creates a [EnumTestEnumNumberEnum] enum from a value and safely checking if it exists.
  factory EnumTestEnumNumberEnum.$safe(double value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  static const $reflection = EnumReflection<EnumTestEnumNumberEnum, double>(
    PrimitiveReflection.fordouble,
    members: [
      
        EnumMemberReflection(dartName: r'number11', oasValue: 1.1, value: EnumTestEnumNumberEnum.number11()),
      
        EnumMemberReflection(dartName: r'numberNegative12', oasValue: -1.2, value: EnumTestEnumNumberEnum.numberNegative12()),
      
    ],
  );

  factory EnumTestEnumNumberEnum.deserialize(Object? value, [SerializationContext context = const SerializationContext.json()]) {
    return $reflection.deserializeFunction(value, context);
  }

  static bool canDeserialize(Object? value, [SerializationContext context = const SerializationContext.json()]) {
    return $reflection.canDeserializeFunction(value,context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json()]) {
    return $reflection.serializeFunction(this, context);
  }

  /// Creates a [EnumTestEnumNumberEnum] enum from a value without checking if it exists.
  const EnumTestEnumNumberEnum.$unsafe(double value) : this._(value);

  /// All possible values of the enum.
  static const List<EnumTestEnumNumberEnum> values = [
    EnumTestEnumNumberEnum.number11(),
    EnumTestEnumNumberEnum.numberNegative12(),
    
  ];
}














