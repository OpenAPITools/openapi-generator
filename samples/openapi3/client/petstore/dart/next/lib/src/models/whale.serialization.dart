// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'whale.dart';


//class serialization

Map<String, dynamic> _$WhaleToMap(Whale instance) {
  final _reflection = WhaleReflection.instance;
  return <String, dynamic>{
    if (instance.hasBaleen.isDefined)
    _reflection.hasBaleen.oasName: (
            bool
 v) {
      return v;
    }(instance.hasBaleen.valueRequired),
    if (instance.hasTeeth.isDefined)
    _reflection.hasTeeth.oasName: (
            bool
 v) {
      return v;
    }(instance.hasTeeth.valueRequired),
    
    _reflection.className.oasName: (
            String
 v) {
      return v;
    }(instance.className),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

Whale _$WhaleFromMap(Map<String, dynamic> src) {
  final _reflection = WhaleReflection.instance;
  return Whale.$all(
    hasBaleen: src.getOrUndefinedMapped(_reflection.hasBaleen.oasName, (v) => 
(

    
            
                    v as bool
            

)


),
hasTeeth: src.getOrUndefinedMapped(_reflection.hasTeeth.oasName, (v) => 
(

    
            
                    v as bool
            

)


),
className: src.getRequiredMapped(_reflection.className.oasName, (v) => 
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

bool _$WhaleCanFromMap(Map<String, dynamic> src) {
  final _reflection = WhaleReflection.instance;
  if (!src.getOrUndefined(_reflection.hasBaleen.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is bool
),
    unDefined: () => !_reflection.hasBaleen.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.hasTeeth.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is bool
),
    unDefined: () => !_reflection.hasTeeth.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.className.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.className.required,
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
Whale _$WhaleDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$WhaleFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$WhaleCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$WhaleCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Map<String,dynamic> _$WhaleSerialize(Whale src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$WhaleToXml(Whale instance) {
  final reflection = WhaleXmlReflection.instance;
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

Whale _$WhaleFromXml(XmlElement src) {
  final reflection = WhaleXmlReflection.instance;
  return Whale.$all(

  );
}
*/

