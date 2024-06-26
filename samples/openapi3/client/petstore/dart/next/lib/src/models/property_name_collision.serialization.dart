// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'property_name_collision.dart';


//class serialization

Map<String, dynamic> _$PropertyNameCollisionToMap(PropertyNameCollision instance) {
  final _reflection = PropertyNameCollisionReflection.instance;
  return <String, dynamic>{
    if (instance.$type.isDefined)
    _reflection.$type.oasName: (
            String
 v) {
      return v;
    }(instance.$type.valueRequired),
    if (instance.type.isDefined)
    _reflection.type.oasName: (
            String
 v) {
      return v;
    }(instance.type.valueRequired),
    if (instance.type$.isDefined)
    _reflection.type$.oasName: (
            String
 v) {
      return v;
    }(instance.type$.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

PropertyNameCollision _$PropertyNameCollisionFromMap(Map<String, dynamic> src) {
  final _reflection = PropertyNameCollisionReflection.instance;
  return PropertyNameCollision.$all(
    $type: src.getOrUndefinedMapped(_reflection.$type.oasName, (v) => 
(

    
            
                    v as String
            

)


),
type: src.getOrUndefinedMapped(_reflection.type.oasName, (v) => 
(

    
            
                    v as String
            

)


),
type$: src.getOrUndefinedMapped(_reflection.type$.oasName, (v) => 
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

bool _$PropertyNameCollisionCanFromMap(Map<String, dynamic> src) {
  final _reflection = PropertyNameCollisionReflection.instance;
  if (!src.getOrUndefined(_reflection.$type.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.$type.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.type.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.type.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.type$.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.type$.required,
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
PropertyNameCollision _$PropertyNameCollisionDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$PropertyNameCollisionFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$PropertyNameCollisionCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$PropertyNameCollisionCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Object? _$PropertyNameCollisionSerialize(PropertyNameCollision src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$PropertyNameCollisionToXml(PropertyNameCollision instance) {
  final reflection = PropertyNameCollisionXmlReflection.instance;
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

PropertyNameCollision _$PropertyNameCollisionFromXml(XmlElement src) {
  final reflection = PropertyNameCollisionXmlReflection.instance;
  return PropertyNameCollision.$all(

  );
}
*/

