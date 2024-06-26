// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'banana.dart';


//class serialization

Map<String, dynamic> _$BananaToMap(Banana instance) {
  final _reflection = BananaReflection.instance;
  return <String, dynamic>{
    if (instance.lengthCm.isDefined)
    _reflection.lengthCm.oasName: (
            num
 v) {
      return v;
    }(instance.lengthCm.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

Banana _$BananaFromMap(Map<String, dynamic> src) {
  final _reflection = BananaReflection.instance;
  return Banana.$all(
    lengthCm: src.getOrUndefinedMapped(_reflection.lengthCm.oasName, (v) => 
(

    
            
                    v as num
            

)


),
    additionalProperties: AdditionalProperties(src.except(_reflection.knownKeys).map((key, v) => MapEntry(key, 
(
v

)
))),
    
  );
}

bool _$BananaCanFromMap(Map<String, dynamic> src) {
  final _reflection = BananaReflection.instance;
  if (!src.getOrUndefined(_reflection.lengthCm.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is num
),
    unDefined: () => !_reflection.lengthCm.required,
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
Banana _$BananaDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$BananaFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$BananaCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$BananaCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Object? _$BananaSerialize(Banana src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$BananaToXml(Banana instance) {
  final reflection = BananaXmlReflection.instance;
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

Banana _$BananaFromXml(XmlElement src) {
  final reflection = BananaXmlReflection.instance;
  return Banana.$all(

  );
}
*/

