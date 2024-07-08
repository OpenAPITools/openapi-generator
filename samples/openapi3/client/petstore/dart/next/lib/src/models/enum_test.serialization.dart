// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'enum_test.dart';


//class serialization

Map<String, dynamic> _$EnumTestToMap(EnumTest instance) {
  final _reflection = EnumTestReflection.instance;
  return <String, dynamic>{
    if (instance.enumString.isDefined)
    _reflection.enumStringPart.oasName: (
            EnumTestEnumStringEnum

 v) {
      return v.value;
    }(instance.enumString.valueRequired),
    
    _reflection.enumStringRequiredPart.oasName: (
            EnumTestEnumStringRequiredEnum

 v) {
      return v.value;
    }(instance.enumStringRequired),
    if (instance.enumInteger.isDefined)
    _reflection.enumIntegerPart.oasName: (
            EnumTestEnumIntegerEnum

 v) {
      return v.value;
    }(instance.enumInteger.valueRequired),
    if (instance.enumIntegerOnly.isDefined)
    _reflection.enumIntegerOnlyPart.oasName: (
            EnumTestEnumIntegerOnlyEnum

 v) {
      return v.value;
    }(instance.enumIntegerOnly.valueRequired),
    if (instance.enumNumber.isDefined)
    _reflection.enumNumberPart.oasName: (
            EnumTestEnumNumberEnum

 v) {
      return v.value;
    }(instance.enumNumber.valueRequired),
    if (instance.outerEnum.isDefined)
    _reflection.outerEnumPart.oasName: (
            OuterEnum

? v) {
      return v;
    }(instance.outerEnum.valueRequired),
    if (instance.outerEnumInteger.isDefined)
    _reflection.outerEnumIntegerPart.oasName: (
            OuterEnumInteger

 v) {
      return v;
    }(instance.outerEnumInteger.valueRequired),
    if (instance.outerEnumDefaultValue.isDefined)
    _reflection.outerEnumDefaultValuePart.oasName: (
            OuterEnumDefaultValue

 v) {
      return v;
    }(instance.outerEnumDefaultValue.valueRequired),
    if (instance.outerEnumIntegerDefaultValue.isDefined)
    _reflection.outerEnumIntegerDefaultValuePart.oasName: (
            OuterEnumIntegerDefaultValue

 v) {
      return v;
    }(instance.outerEnumIntegerDefaultValue.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

EnumTest _$EnumTestFromMap(Map<String, dynamic> src) {
  const _reflection = EnumTestReflection.instance;
  return EnumTest.$all(
    enumString: src.getOrUndefinedMapped(_reflection.enumStringPart.oasName, (v) => 
(

            
                    EnumTestEnumStringEnum.$safe(( v is String ? v as String :




throwArgumentMismatch(String, v)

))

)


),
enumStringRequired: src.getRequiredMapped(_reflection.enumStringRequiredPart.oasName, (v) => 
(

            
                    EnumTestEnumStringRequiredEnum.$safe(( v is String ? v as String :




throwArgumentMismatch(String, v)

))

)


),
enumInteger: src.getOrUndefinedMapped(_reflection.enumIntegerPart.oasName, (v) => 
(

            
                    EnumTestEnumIntegerEnum.$safe(( v is int ? v as int :
int.parse(v.toString())



))

)


),
enumIntegerOnly: src.getOrUndefinedMapped(_reflection.enumIntegerOnlyPart.oasName, (v) => 
(

            
                    EnumTestEnumIntegerOnlyEnum.$safe(( v is int ? v as int :
int.parse(v.toString())



))

)


),
enumNumber: src.getOrUndefinedMapped(_reflection.enumNumberPart.oasName, (v) => 
(

            
                    EnumTestEnumNumberEnum.$safe(( v is double ? v as double :
double.parse(v.toString())



))

)


),
outerEnum: src.getOrUndefinedMapped(_reflection.outerEnumPart.oasName, (v) => 
(

            
                    (v == null ? null :  v is OuterEnum ? v as OuterEnum :




throwArgumentMismatch(OuterEnum, v)

)

)


),
outerEnumInteger: src.getOrUndefinedMapped(_reflection.outerEnumIntegerPart.oasName, (v) => 
(

            
                    ( v is OuterEnumInteger ? v as OuterEnumInteger :




throwArgumentMismatch(OuterEnumInteger, v)

)

)


),
outerEnumDefaultValue: src.getOrUndefinedMapped(_reflection.outerEnumDefaultValuePart.oasName, (v) => 
(

            
                    ( v is OuterEnumDefaultValue ? v as OuterEnumDefaultValue :




throwArgumentMismatch(OuterEnumDefaultValue, v)

)

)


),
outerEnumIntegerDefaultValue: src.getOrUndefinedMapped(_reflection.outerEnumIntegerDefaultValuePart.oasName, (v) => 
(

            
                    ( v is OuterEnumIntegerDefaultValue ? v as OuterEnumIntegerDefaultValue :




throwArgumentMismatch(OuterEnumIntegerDefaultValue, v)

)

)


),
    additionalProperties: AdditionalProperties(src.except(_reflection.knownKeys).map((key, v) => MapEntry(key, 
(
v

)
))),
    
  );
}

bool _$EnumTestCanFromMap(Map<String, dynamic> src) {
  final _reflection = EnumTestReflection.instance;

  if (!src.getOrUndefined(_reflection.enumStringPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
     && EnumTestEnumStringEnum.canDeserialize(v)
)
),
    unDefined: () => !_reflection.enumStringPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.enumStringRequiredPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
     && EnumTestEnumStringRequiredEnum.canDeserialize(v)
)
),
    unDefined: () => !_reflection.enumStringRequiredPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.enumIntegerPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is int
     || (int.tryParse(v.toString()) != null)
    
    
     && EnumTestEnumIntegerEnum.canDeserialize(v)
)
),
    unDefined: () => !_reflection.enumIntegerPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.enumIntegerOnlyPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is int
     || (int.tryParse(v.toString()) != null)
    
    
     && EnumTestEnumIntegerOnlyEnum.canDeserialize(v)
)
),
    unDefined: () => !_reflection.enumIntegerOnlyPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.enumNumberPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is double
     || (double.tryParse(v.toString()) != null)
    
    
     && EnumTestEnumNumberEnum.canDeserialize(v)
)
),
    unDefined: () => !_reflection.enumNumberPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.outerEnumPart.oasName).split<bool>(
    defined: (v) => v == null ? true :
(

    
            
            (v is OuterEnum
    
    
    
    
)
),
    unDefined: () => !_reflection.outerEnumPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.outerEnumIntegerPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is OuterEnumInteger
    
    
    
    
)
),
    unDefined: () => !_reflection.outerEnumIntegerPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.outerEnumDefaultValuePart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is OuterEnumDefaultValue
    
    
    
    
)
),
    unDefined: () => !_reflection.outerEnumDefaultValuePart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.outerEnumIntegerDefaultValuePart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is OuterEnumIntegerDefaultValue
    
    
    
    
)
),
    unDefined: () => !_reflection.outerEnumIntegerDefaultValuePart.required,
)) {
    return false;
  }
  if (!src.except(_reflection.knownKeys).values.every((v) => v == null ? true :
(
true
))) {
    return false;
  }

  return true;
}

/// Deserializes a primitive Object (num, String, List, Map).
EnumTest _$EnumTestDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$EnumTestFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$EnumTestCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$EnumTestCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Map<String, dynamic> _$EnumTestSerialize(EnumTest src) {
  Map<String, dynamic> initialResult = () {
    
      return _$EnumTestToMap(src);
    
  }();
  return initialResult;
}


/*
XmlElement _$EnumTestToXml(EnumTest instance) {
  final reflection = EnumTestXmlReflection.instance;
  final result = XmlElement(
    XmlName(reflection.oasName, reflection.oasNamespace),
    //attributes
    [

    ],
    //elements
    [
    ],
  );
  return result;
}

EnumTest _$EnumTestFromXml(XmlElement src) {
  final reflection = EnumTestXmlReflection.instance;
  return EnumTest.$all(

  );
}
*/

