// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'apple.dart';


//class serialization

Map<String, dynamic> _$AppleToMap(Apple instance) {
  final _reflection = AppleReflection.instance;
  return <String, dynamic>{
    if (instance.cultivar.isDefined)
    _reflection.cultivar.oasName: (
            String
 v) {
      return v;
    }(instance.cultivar.valueRequired),
    if (instance.origin.isDefined)
    _reflection.origin.oasName: (
            String
 v) {
      return v;
    }(instance.origin.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

Apple _$AppleFromMap(Map<String, dynamic> src) {
  final _reflection = AppleReflection.instance;
  return Apple.$all(
    cultivar: src.getOrUndefinedMapped(_reflection.cultivar.oasName, (v) => 
(

    
            
                    v as String
            

)


),
origin: src.getOrUndefinedMapped(_reflection.origin.oasName, (v) => 
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

bool _$AppleCanFromMap(Map<String, dynamic> src) {
  final _reflection = AppleReflection.instance;
  if (!src.getOrUndefined(_reflection.cultivar.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.cultivar.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.origin.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.origin.required,
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
Apple _$AppleDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$AppleFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$AppleCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$AppleCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Map<String,dynamic> _$AppleSerialize(Apple src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$AppleToXml(Apple instance) {
  final reflection = AppleXmlReflection.instance;
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

Apple _$AppleFromXml(XmlElement src) {
  final reflection = AppleXmlReflection.instance;
  return Apple.$all(

  );
}
*/

