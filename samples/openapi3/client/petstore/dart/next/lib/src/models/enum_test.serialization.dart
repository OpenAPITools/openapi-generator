// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'enum_test.dart';


//class serialization

Map<String, dynamic> _$EnumTestToMap(EnumTest instance) {
  final _reflection = EnumTestReflection.instance;
  return <String, dynamic>{
    if (instance.enumString.isDefined)
    _reflection.enumString.oasName: (
            EnumTestEnumStringEnum
 v) {
      return v.value;
    }(instance.enumString.valueRequired),
    
    _reflection.enumStringRequired.oasName: (
            EnumTestEnumStringRequiredEnum
 v) {
      return v.value;
    }(instance.enumStringRequired),
    if (instance.enumInteger.isDefined)
    _reflection.enumInteger.oasName: (
            EnumTestEnumIntegerEnum
 v) {
      return v.value;
    }(instance.enumInteger.valueRequired),
    if (instance.enumIntegerOnly.isDefined)
    _reflection.enumIntegerOnly.oasName: (
            EnumTestEnumIntegerOnlyEnum
 v) {
      return v.value;
    }(instance.enumIntegerOnly.valueRequired),
    if (instance.enumNumber.isDefined)
    _reflection.enumNumber.oasName: (
            EnumTestEnumNumberEnum
 v) {
      return v.value;
    }(instance.enumNumber.valueRequired),
    if (instance.outerEnum.isDefined)
    _reflection.outerEnum.oasName: (
            OuterEnum
? v) {
      return v;
    }(instance.outerEnum.valueRequired),
    if (instance.outerEnumInteger.isDefined)
    _reflection.outerEnumInteger.oasName: (
            OuterEnumInteger
 v) {
      return v;
    }(instance.outerEnumInteger.valueRequired),
    if (instance.outerEnumDefaultValue.isDefined)
    _reflection.outerEnumDefaultValue.oasName: (
            OuterEnumDefaultValue
 v) {
      return v;
    }(instance.outerEnumDefaultValue.valueRequired),
    if (instance.outerEnumIntegerDefaultValue.isDefined)
    _reflection.outerEnumIntegerDefaultValue.oasName: (
            OuterEnumIntegerDefaultValue
 v) {
      return v;
    }(instance.outerEnumIntegerDefaultValue.valueRequired),
    
    
  };
}

EnumTest _$EnumTestFromMap(Map<String, dynamic> src) {
  final _reflection = EnumTestReflection.instance;
  return EnumTest.$all(
    enumString: src.getOrUndefinedMapped(_reflection.enumString.oasName, (v) => 
(

    
            
                    EnumTestEnumStringEnum.$safe(v as String)
            

)


),
enumStringRequired: src.getRequiredMapped(_reflection.enumStringRequired.oasName, (v) => 
(

    
            
                    EnumTestEnumStringRequiredEnum.$safe(v as String)
            

)


),
enumInteger: src.getOrUndefinedMapped(_reflection.enumInteger.oasName, (v) => 
(

    
            
                    EnumTestEnumIntegerEnum.$safe(v as int)
            

)


),
enumIntegerOnly: src.getOrUndefinedMapped(_reflection.enumIntegerOnly.oasName, (v) => 
(

    
            
                    EnumTestEnumIntegerOnlyEnum.$safe(v as int)
            

)


),
enumNumber: src.getOrUndefinedMapped(_reflection.enumNumber.oasName, (v) => 
(

    
            
                    EnumTestEnumNumberEnum.$safe(v as double)
            

)


),
outerEnum: src.getOrUndefinedMapped(_reflection.outerEnum.oasName, (v) => 
(

    
            
                    v as OuterEnum
            ?

)


),
outerEnumInteger: src.getOrUndefinedMapped(_reflection.outerEnumInteger.oasName, (v) => 
(

    
            
                    v as OuterEnumInteger
            

)


),
outerEnumDefaultValue: src.getOrUndefinedMapped(_reflection.outerEnumDefaultValue.oasName, (v) => 
(

    
            
                    v as OuterEnumDefaultValue
            

)


),
outerEnumIntegerDefaultValue: src.getOrUndefinedMapped(_reflection.outerEnumIntegerDefaultValue.oasName, (v) => 
(

    
            
                    v as OuterEnumIntegerDefaultValue
            

)


),
    
    
  );
}

bool _$EnumTestCanFromMap(Map<String, dynamic> src) {
  final _reflection = EnumTestReflection.instance;
  if (!src.getOrUndefined(_reflection.enumString.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.enumString.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.enumStringRequired.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.enumStringRequired.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.enumInteger.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is int
),
    unDefined: () => !_reflection.enumInteger.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.enumIntegerOnly.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is int
),
    unDefined: () => !_reflection.enumIntegerOnly.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.enumNumber.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is double
),
    unDefined: () => !_reflection.enumNumber.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.outerEnum.oasName).split<bool>(
    defined: (v) => v == null ? true :
(

    
            
            v is OuterEnum
),
    unDefined: () => !_reflection.outerEnum.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.outerEnumInteger.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is OuterEnumInteger
),
    unDefined: () => !_reflection.outerEnumInteger.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.outerEnumDefaultValue.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is OuterEnumDefaultValue
),
    unDefined: () => !_reflection.outerEnumDefaultValue.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.outerEnumIntegerDefaultValue.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is OuterEnumIntegerDefaultValue
),
    unDefined: () => !_reflection.outerEnumIntegerDefaultValue.required,
)) {
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
Object? _$EnumTestSerialize(EnumTest src) {
  
  return src.toMap();
  
  
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

