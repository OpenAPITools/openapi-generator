// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'animal.dart';


//class serialization

Map<String, dynamic> _$AnimalToMap(Animal instance) {
  final _reflection = AnimalReflection.instance;
  return <String, dynamic>{
    
    _reflection.className.oasName: (
            String
 v) {
      return v;
    }(instance.className),
    if (instance.color.isDefined)
    _reflection.color.oasName: (
            String
 v) {
      return v;
    }(instance.color.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

Animal _$AnimalFromMap(Map<String, dynamic> src) {
  final _reflection = AnimalReflection.instance;
  return Animal.$all(
    className: src.getRequiredMapped(_reflection.className.oasName, (v) => 
(

    
            
                    v as String
            

)


),
color: src.getOrUndefinedMapped(_reflection.color.oasName, (v) => 
(

    
            
                    v as String
            

)


),
    additionalProperties: AdditionalProperties(src.except(_reflection.knownKeys).map((key, v) => MapEntry(key, 
(
v

)
))),
    
  );
}

bool _$AnimalCanFromMap(Map<String, dynamic> src) {
  final _reflection = AnimalReflection.instance;
  if (!src.getOrUndefined(_reflection.className.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.className.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.color.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.color.required,
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
Animal _$AnimalDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$AnimalFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$AnimalCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$AnimalCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Map<String,dynamic> _$AnimalSerialize(Animal src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$AnimalToXml(Animal instance) {
  final reflection = AnimalXmlReflection.instance;
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

Animal _$AnimalFromXml(XmlElement src) {
  final reflection = AnimalXmlReflection.instance;
  return Animal.$all(

  );
}
*/

