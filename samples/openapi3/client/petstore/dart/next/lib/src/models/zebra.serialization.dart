// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'zebra.dart';


//class serialization

Map<String, dynamic> _$ZebraToMap(Zebra instance) {
  final _reflection = ZebraReflection.instance;
  return <String, dynamic>{
    if (instance.type.isDefined)
    _reflection.type.oasName: (
            ZebraTypeEnum
 v) {
      return v.value;
    }(instance.type.valueRequired),
    
    _reflection.className.oasName: (
            String
 v) {
      return v;
    }(instance.className),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

Zebra _$ZebraFromMap(Map<String, dynamic> src) {
  final _reflection = ZebraReflection.instance;
  return Zebra.$all(
    type: src.getOrUndefinedMapped(_reflection.type.oasName, (v) => 
(

    
            
                    ZebraTypeEnum.$safe(v as String)
            

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

bool _$ZebraCanFromMap(Map<String, dynamic> src) {
  final _reflection = ZebraReflection.instance;
  if (!src.getOrUndefined(_reflection.type.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.type.required,
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
Zebra _$ZebraDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$ZebraFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$ZebraCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$ZebraCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Object? _$ZebraSerialize(Zebra src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$ZebraToXml(Zebra instance) {
  final reflection = ZebraXmlReflection.instance;
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

Zebra _$ZebraFromXml(XmlElement src) {
  final reflection = ZebraXmlReflection.instance;
  return Zebra.$all(

  );
}
*/

