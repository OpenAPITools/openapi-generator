// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'map_test.dart';


//class serialization

Map<String, dynamic> _$MapTestToMap(MapTest instance) {
  final _reflection = MapTestReflection.instance;
  return <String, dynamic>{
    if (instance.mapMapOfString.isDefined)
    _reflection.mapMapOfStringPart.oasName: (
    Map<String, 
        
    Map<String, 
        
            String

>

>

 v) {
      return v.map((k,v) => MapEntry(k, v.map((k,v) => MapEntry(k, v))));
    }(instance.mapMapOfString.valueRequired),
    if (instance.mapOfEnumString.isDefined)
    _reflection.mapOfEnumStringPart.oasName: (
    Map<String, 
        
            MapTestMapOfEnumStringEnum

>

 v) {
      return v.map((k,v) => MapEntry(k, v.value));
    }(instance.mapOfEnumString.valueRequired),
    if (instance.directMap.isDefined)
    _reflection.directMapPart.oasName: (
    Map<String, 
        
            bool

>

 v) {
      return v.map((k,v) => MapEntry(k, v));
    }(instance.directMap.valueRequired),
    if (instance.indirectMap.isDefined)
    _reflection.indirectMapPart.oasName: (
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
  const _reflection = MapTestReflection.instance;
  return MapTest.$all(
    mapMapOfString: src.getOrUndefinedMapped(_reflection.mapMapOfStringPart.oasName, (v) => 
(

            v as Map<String, dynamic>
            
            

)
.map((k,v) => MapEntry(k, 
(

            v as Map<String, dynamic>
            
            

)
.map((k,v) => MapEntry(k, 
(

            
                    ( v is String ? v as String :




throwArgumentMismatch(String, v)

)

)


))

))

),
mapOfEnumString: src.getOrUndefinedMapped(_reflection.mapOfEnumStringPart.oasName, (v) => 
(

            v as Map<String, dynamic>
            
            

)
.map((k,v) => MapEntry(k, 
(

            
                    MapTestMapOfEnumStringEnum.$safe(( v is String ? v as String :




throwArgumentMismatch(String, v)

))

)


))

),
directMap: src.getOrUndefinedMapped(_reflection.directMapPart.oasName, (v) => 
(

            v as Map<String, dynamic>
            
            

)
.map((k,v) => MapEntry(k, 
(

            
                    ( v is bool ? v as bool :

bool.parse(v.toString())


)

)


))

),
indirectMap: src.getOrUndefinedMapped(_reflection.indirectMapPart.oasName, (v) => 
(

            v as Map<String, dynamic>
            
            

)
.map((k,v) => MapEntry(k, 
(

            
                    ( v is bool ? v as bool :

bool.parse(v.toString())


)

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

  if (!src.getOrUndefined(_reflection.mapMapOfStringPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            v is Map<String, dynamic> && v.values.every((v) => v == null ? false :
(

    
            v is Map<String, dynamic> && v.values.every((v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
))
            
))
            
),
    unDefined: () => !_reflection.mapMapOfStringPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.mapOfEnumStringPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            v is Map<String, dynamic> && v.values.every((v) => v == null ? false :
(

    
            
            (v is String
    
    
    
     && MapTestMapOfEnumStringEnum.canDeserialize(v)
)
))
            
),
    unDefined: () => !_reflection.mapOfEnumStringPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.directMapPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            v is Map<String, dynamic> && v.values.every((v) => v == null ? false :
(

    
            
            (v is bool
    
     || (bool.tryParse(v.toString()) != null)
    
    
)
))
            
),
    unDefined: () => !_reflection.directMapPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.indirectMapPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            v is Map<String, dynamic> && v.values.every((v) => v == null ? false :
(

    
            
            (v is bool
    
     || (bool.tryParse(v.toString()) != null)
    
    
)
))
            
),
    unDefined: () => !_reflection.indirectMapPart.required,
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
Map<String, dynamic> _$MapTestSerialize(MapTest src) {
  Map<String, dynamic> initialResult = () {
    
      return _$MapTestToMap(src);
    
  }();
  return initialResult;
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

