// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'array_default.dart';


//class serialization

Map<String, dynamic> _$ArrayDefaultToMap(ArrayDefault instance) {
  final _reflection = ArrayDefaultReflection.instance;
  return <String, dynamic>{
    if (instance.withDefaultEmptyBracket.isDefined)
    _reflection.withDefaultEmptyBracket.oasName: (
    List<
        
            String
>
 v) {
      return v.map((v) => v).toList();
    }(instance.withDefaultEmptyBracket.valueRequired),
    if (instance.withoutDefault.isDefined)
    _reflection.withoutDefault.oasName: (
    List<
        
            String
>
 v) {
      return v.map((v) => v).toList();
    }(instance.withoutDefault.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

ArrayDefault _$ArrayDefaultFromMap(Map<String, dynamic> src) {
  final _reflection = ArrayDefaultReflection.instance;
  return ArrayDefault.$all(
    withDefaultEmptyBracket: src.getOrUndefinedMapped(_reflection.withDefaultEmptyBracket.oasName, (v) => 
(

    
            
            v as List
            

)

.map((v) => 
(

    
            
                    v as String
            

)


).toList()
),
withoutDefault: src.getOrUndefinedMapped(_reflection.withoutDefault.oasName, (v) => 
(

    
            
            v as List
            

)

.map((v) => 
(

    
            
                    v as String
            

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

bool _$ArrayDefaultCanFromMap(Map<String, dynamic> src) {
  final _reflection = ArrayDefaultReflection.instance;
  if (!src.getOrUndefined(_reflection.withDefaultEmptyBracket.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is List && v.every((v) => v == null ? false :
(

    
            
            v is String
))
),
    unDefined: () => !_reflection.withDefaultEmptyBracket.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.withoutDefault.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is List && v.every((v) => v == null ? false :
(

    
            
            v is String
))
),
    unDefined: () => !_reflection.withoutDefault.required,
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
ArrayDefault _$ArrayDefaultDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$ArrayDefaultFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$ArrayDefaultCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$ArrayDefaultCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Object? _$ArrayDefaultSerialize(ArrayDefault src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$ArrayDefaultToXml(ArrayDefault instance) {
  final reflection = ArrayDefaultXmlReflection.instance;
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

ArrayDefault _$ArrayDefaultFromXml(XmlElement src) {
  final reflection = ArrayDefaultXmlReflection.instance;
  return ArrayDefault.$all(

  );
}
*/

