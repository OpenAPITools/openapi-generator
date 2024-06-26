// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'enum_arrays.dart';


//class serialization

Map<String, dynamic> _$EnumArraysToMap(EnumArrays instance) {
  final _reflection = EnumArraysReflection.instance;
  return <String, dynamic>{
    if (instance.justSymbol.isDefined)
    _reflection.justSymbol.oasName: (
            EnumArraysJustSymbolEnum
 v) {
      return v.value;
    }(instance.justSymbol.valueRequired),
    if (instance.arrayEnum.isDefined)
    _reflection.arrayEnum.oasName: (
    List<
        
            EnumArraysArrayEnumEnum
>
 v) {
      return v.map((v) => v.value).toList();
    }(instance.arrayEnum.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

EnumArrays _$EnumArraysFromMap(Map<String, dynamic> src) {
  final _reflection = EnumArraysReflection.instance;
  return EnumArrays.$all(
    justSymbol: src.getOrUndefinedMapped(_reflection.justSymbol.oasName, (v) => 
(

    
            
                    EnumArraysJustSymbolEnum.$safe(v as String)
            

)


),
arrayEnum: src.getOrUndefinedMapped(_reflection.arrayEnum.oasName, (v) => 
(

    
            
            v as List
            

)

.map((v) => 
(

    
            
                    EnumArraysArrayEnumEnum.$safe(v as String)
            

)


).toList()
),
    additionalProperties: AdditionalProperties(src.except(_reflection.knownKeys).map((key, v) => MapEntry(key, 
(
v

)
))),
    
  );
}

bool _$EnumArraysCanFromMap(Map<String, dynamic> src) {
  final _reflection = EnumArraysReflection.instance;
  if (!src.getOrUndefined(_reflection.justSymbol.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.justSymbol.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.arrayEnum.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is List && v.every((v) => v == null ? false :
(

    
            
            v is String
))
),
    unDefined: () => !_reflection.arrayEnum.required,
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
EnumArrays _$EnumArraysDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$EnumArraysFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$EnumArraysCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$EnumArraysCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Object? _$EnumArraysSerialize(EnumArrays src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$EnumArraysToXml(EnumArrays instance) {
  final reflection = EnumArraysXmlReflection.instance;
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

EnumArrays _$EnumArraysFromXml(XmlElement src) {
  final reflection = EnumArraysXmlReflection.instance;
  return EnumArrays.$all(

  );
}
*/

