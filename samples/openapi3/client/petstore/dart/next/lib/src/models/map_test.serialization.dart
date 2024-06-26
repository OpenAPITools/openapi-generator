// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'map_test.dart';


//class serialization

Map<String, dynamic> _$MapTestToMap(MapTest instance) {
  final _reflection = MapTestReflection.instance;
  return <String, dynamic>{
    if (instance.mapMapOfString.isDefined)
    _reflection.mapMapOfString.oasName: (
    Map<String, 
        
    Map<String, 
        
            String
>
>
 v) {
      return v.map((k,v) => MapEntry(k, v.map((k,v) => MapEntry(k, v))));
    }(instance.mapMapOfString.valueRequired),
    if (instance.mapOfEnumString.isDefined)
    _reflection.mapOfEnumString.oasName: (
    Map<String, 
        
            MapTestMapOfEnumStringEnum
>
 v) {
      return v.map((k,v) => MapEntry(k, v.value));
    }(instance.mapOfEnumString.valueRequired),
    if (instance.directMap.isDefined)
    _reflection.directMap.oasName: (
    Map<String, 
        
            bool
>
 v) {
      return v.map((k,v) => MapEntry(k, v));
    }(instance.directMap.valueRequired),
    if (instance.indirectMap.isDefined)
    _reflection.indirectMap.oasName: (
    Map<String, 
        
            bool
>
 v) {
      return v.map((k,v) => MapEntry(k, v));
    }(instance.indirectMap.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

MapTest _$MapTestFromMap(Map<String, dynamic> src) {
  final _reflection = MapTestReflection.instance;
  return MapTest.$all(
    mapMapOfString: src.getOrUndefinedMapped(_reflection.mapMapOfString.oasName, (v) => 
(

    
            v as Map<String, dynamic>
            
            

)
.map((k,v) => MapEntry(k, 
(

    
            v as Map<String, dynamic>
            
            

)
.map((k,v) => MapEntry(k, 
(

    
            
                    v as String
            

)


))

))

),
mapOfEnumString: src.getOrUndefinedMapped(_reflection.mapOfEnumString.oasName, (v) => 
(

    
            v as Map<String, dynamic>
            
            

)
.map((k,v) => MapEntry(k, 
(

    
            
                    MapTestMapOfEnumStringEnum.$safe(v as String)
            

)


))

),
directMap: src.getOrUndefinedMapped(_reflection.directMap.oasName, (v) => 
(

    
            v as Map<String, dynamic>
            
            

)
.map((k,v) => MapEntry(k, 
(

    
            
                    v as bool
            

)


))

),
indirectMap: src.getOrUndefinedMapped(_reflection.indirectMap.oasName, (v) => 
(

    
            v as Map<String, dynamic>
            
            

)
.map((k,v) => MapEntry(k, 
(

    
            
                    v as bool
            

)


))

),
    additionalProperties: AdditionalProperties(src.except(_reflection.knownKeys).map((key, v) => MapEntry(key, 
(
v

)
))),
    
  );
}

bool _$MapTestCanFromMap(Map<String, dynamic> src) {
  final _reflection = MapTestReflection.instance;
  if (!src.getOrUndefined(_reflection.mapMapOfString.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            v is Map<String, dynamic> && v.values.every((v) => v == null ? false :
(

    
            v is Map<String, dynamic> && v.values.every((v) => v == null ? false :
(

    
            
            v is String
))
            
))
            
),
    unDefined: () => !_reflection.mapMapOfString.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.mapOfEnumString.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            v is Map<String, dynamic> && v.values.every((v) => v == null ? false :
(

    
            
            v is String
))
            
),
    unDefined: () => !_reflection.mapOfEnumString.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.directMap.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            v is Map<String, dynamic> && v.values.every((v) => v == null ? false :
(

    
            
            v is bool
))
            
),
    unDefined: () => !_reflection.directMap.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.indirectMap.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            v is Map<String, dynamic> && v.values.every((v) => v == null ? false :
(

    
            
            v is bool
))
            
),
    unDefined: () => !_reflection.indirectMap.required,
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
MapTest _$MapTestDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$MapTestFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$MapTestCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$MapTestCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Object? _$MapTestSerialize(MapTest src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$MapTestToXml(MapTest instance) {
  final reflection = MapTestXmlReflection.instance;
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

MapTest _$MapTestFromXml(XmlElement src) {
  final reflection = MapTestXmlReflection.instance;
  return MapTest.$all(

  );
}
*/

