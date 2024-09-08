// Model def

import 'package:petstore_api/_internal.dart';


part 'enum_arrays.reflection.dart';


/// EnumArraysMixin
///
/// Properties:
/// * [justSymbol] 
/// * [arrayEnum] 
mixin EnumArraysMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            EnumArraysJustSymbolEnum
> get justSymbol;
UndefinedWrapper<
    List<
        
            EnumArraysArrayEnumEnum
>
> get arrayEnum;
  
}

/// EnumArrays
///
/// Properties:
/// * [justSymbol] 
/// * [arrayEnum] 
class EnumArrays with
$OpenApiObjectMixin,

EnumArraysMixin {
  @override
  UndefinedWrapper<
            EnumArraysJustSymbolEnum
> justSymbol;
  @override
  UndefinedWrapper<
    List<
        
            EnumArraysArrayEnumEnum
>
> arrayEnum;

  AdditionalProperties<Object
?> additionalProperties;

  

  EnumArrays.$all({
        required this.justSymbol,
    required this.arrayEnum,
    required this.additionalProperties,
    
  });

  EnumArrays({
      this.justSymbol = const UndefinedWrapper
        .undefined()
,
  this.arrayEnum = const UndefinedWrapper
        .undefined()
,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = EnumArraysReflection.instance;
  EnumArraysReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory EnumArrays.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  EnumArrays clone() {
    return $reflection.clone(this);
  }
}


extension type const EnumArraysJustSymbolEnum._(String value) implements String {
      const EnumArraysJustSymbolEnum.greaterThanEqual() : this._(r'>=');
      const EnumArraysJustSymbolEnum.value() : this._(r'$');

  /// Creates a [EnumArraysJustSymbolEnum] enum from a value and safely checking if it exists.
  factory EnumArraysJustSymbolEnum.$safe(String value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  static const $reflection = EnumReflection<EnumArraysJustSymbolEnum, String>(
    PrimitiveReflection.forString,
    members: [
      
        EnumMemberReflection(dartName: r'greaterThanEqual', oasValue: r'>=', value: EnumArraysJustSymbolEnum.greaterThanEqual()),
      
        EnumMemberReflection(dartName: r'value', oasValue: r'$', value: EnumArraysJustSymbolEnum.value()),
      
    ],
  );

  factory EnumArraysJustSymbolEnum.deserialize(Object? value, [SerializationContext context = const SerializationContext.json()]) {
    return $reflection.deserializeFunction(value, context);
  }

  static bool canDeserialize(Object? value, [SerializationContext context = const SerializationContext.json()]) {
    return $reflection.canDeserializeFunction(value,context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json()]) {
    return $reflection.serializeFunction(this, context);
  }

  /// Creates a [EnumArraysJustSymbolEnum] enum from a value without checking if it exists.
  const EnumArraysJustSymbolEnum.$unsafe(String value) : this._(value);

  /// All possible values of the enum.
  static const List<EnumArraysJustSymbolEnum> values = [
    EnumArraysJustSymbolEnum.greaterThanEqual(),
    EnumArraysJustSymbolEnum.value(),
    
  ];
}


extension type const EnumArraysArrayEnumEnum._(String value) implements String {
      const EnumArraysArrayEnumEnum.fish() : this._(r'fish');
      const EnumArraysArrayEnumEnum.crab() : this._(r'crab');

  /// Creates a [EnumArraysArrayEnumEnum] enum from a value and safely checking if it exists.
  factory EnumArraysArrayEnumEnum.$safe(String value) {
    final res = values.where((element) => element.value == value).firstOrNull;
    if (res == null) {
      throw 'Invalid enum value $value';
    }
    return res;
  }

  static const $reflection = EnumReflection<EnumArraysArrayEnumEnum, String>(
    PrimitiveReflection.forString,
    members: [
      
        EnumMemberReflection(dartName: r'fish', oasValue: r'fish', value: EnumArraysArrayEnumEnum.fish()),
      
        EnumMemberReflection(dartName: r'crab', oasValue: r'crab', value: EnumArraysArrayEnumEnum.crab()),
      
    ],
  );

  factory EnumArraysArrayEnumEnum.deserialize(Object? value, [SerializationContext context = const SerializationContext.json()]) {
    return $reflection.deserializeFunction(value, context);
  }

  static bool canDeserialize(Object? value, [SerializationContext context = const SerializationContext.json()]) {
    return $reflection.canDeserializeFunction(value,context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json()]) {
    return $reflection.serializeFunction(this, context);
  }

  /// Creates a [EnumArraysArrayEnumEnum] enum from a value without checking if it exists.
  const EnumArraysArrayEnumEnum.$unsafe(String value) : this._(value);

  /// All possible values of the enum.
  static const List<EnumArraysArrayEnumEnum> values = [
    EnumArraysArrayEnumEnum.fish(),
    EnumArraysArrayEnumEnum.crab(),
    
  ];
}




