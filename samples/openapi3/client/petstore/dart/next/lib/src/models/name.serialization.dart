// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'name.dart';


//class serialization

Map<String, dynamic> _$NameToMap(Name instance) {
  final _reflection = NameReflection.instance;
  return <String, dynamic>{
    
    _reflection.name.oasName: (
            int
 v) {
      return v;
    }(instance.name),
    if (instance.snakeCase.isDefined)
    _reflection.snakeCase.oasName: (
            int
 v) {
      return v;
    }(instance.snakeCase.valueRequired),
    if (instance.property.isDefined)
    _reflection.property.oasName: (
            String
 v) {
      return v;
    }(instance.property.valueRequired),
    if (instance.$123number.isDefined)
    _reflection.$123number.oasName: (
            int
 v) {
      return v;
    }(instance.$123number.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

Name _$NameFromMap(Map<String, dynamic> src) {
  final _reflection = NameReflection.instance;
  return Name.$all(
    name: src.getRequiredMapped(_reflection.name.oasName, (v) => 
(

    
            
                    v as int
            

)


),
snakeCase: src.getOrUndefinedMapped(_reflection.snakeCase.oasName, (v) => 
(

    
            
                    v as int
            

)


),
property: src.getOrUndefinedMapped(_reflection.property.oasName, (v) => 
(

    
            
                    v as String
            

)


),
$123number: src.getOrUndefinedMapped(_reflection.$123number.oasName, (v) => 
(

    
            
                    v as int
            

)


),
    additionalProperties: AdditionalProperties(src.except(_reflection.knownKeys).map((key, v) => MapEntry(key, 
(
v

)
))),
    
  );
}

bool _$NameCanFromMap(Map<String, dynamic> src) {
  final _reflection = NameReflection.instance;
  if (!src.getOrUndefined(_reflection.name.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is int
),
    unDefined: () => !_reflection.name.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.snakeCase.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is int
),
    unDefined: () => !_reflection.snakeCase.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.property.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.property.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.$123number.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is int
),
    unDefined: () => !_reflection.$123number.required,
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
Name _$NameDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$NameFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$NameCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$NameCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Object? _$NameSerialize(Name src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$NameToXml(Name instance) {
  final reflection = NameXmlReflection.instance;
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

Name _$NameFromXml(XmlElement src) {
  final reflection = NameXmlReflection.instance;
  return Name.$all(

  );
}
*/

