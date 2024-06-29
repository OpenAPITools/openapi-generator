// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'special_model_name.dart';


//class serialization

Map<String, dynamic> _$SpecialModelNameToMap(SpecialModelName instance) {
  final _reflection = SpecialModelNameReflection.instance;
  return <String, dynamic>{
    if (instance.$specialPropertyName.isDefined)
    _reflection.$specialPropertyName.oasName: (
            int
 v) {
      return v;
    }(instance.$specialPropertyName.valueRequired),
    if (instance.specialModelName.isDefined)
    _reflection.specialModelName.oasName: (
            String
 v) {
      return v;
    }(instance.specialModelName.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

SpecialModelName _$SpecialModelNameFromMap(Map<String, dynamic> src) {
  final _reflection = SpecialModelNameReflection.instance;
  return SpecialModelName.$all(
    $specialPropertyName: src.getOrUndefinedMapped(_reflection.$specialPropertyName.oasName, (v) => 
(

    
            
                    v as int
            

)


),
specialModelName: src.getOrUndefinedMapped(_reflection.specialModelName.oasName, (v) => 
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

bool _$SpecialModelNameCanFromMap(Map<String, dynamic> src) {
  final _reflection = SpecialModelNameReflection.instance;
  if (!src.getOrUndefined(_reflection.$specialPropertyName.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is int
),
    unDefined: () => !_reflection.$specialPropertyName.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.specialModelName.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.specialModelName.required,
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
SpecialModelName _$SpecialModelNameDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$SpecialModelNameFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$SpecialModelNameCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$SpecialModelNameCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Map<String,dynamic> _$SpecialModelNameSerialize(SpecialModelName src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$SpecialModelNameToXml(SpecialModelName instance) {
  final reflection = SpecialModelNameXmlReflection.instance;
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

SpecialModelName _$SpecialModelNameFromXml(XmlElement src) {
  final reflection = SpecialModelNameXmlReflection.instance;
  return SpecialModelName.$all(

  );
}
*/

