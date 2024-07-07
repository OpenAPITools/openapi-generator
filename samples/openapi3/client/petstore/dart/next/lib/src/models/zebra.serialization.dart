// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'zebra.dart';


//class serialization

Map<String, dynamic> _$ZebraToMap(Zebra instance) {
  final _reflection = ZebraReflection.instance;
  return <String, dynamic>{
    if (instance.type.isDefined)
    _reflection.typePart.oasName: (
            ZebraTypeEnum
 v) {
      return v.value;
    }(instance.type.valueRequired),
    
    _reflection.classNamePart.oasName: (
            String
 v) {
      return v;
    }(instance.className),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

Zebra _$ZebraFromMap(Map<String, dynamic> src) {
  const _reflection = ZebraReflection.instance;
  return Zebra.$all(
    type: src.getOrUndefinedMapped(_reflection.typePart.oasName, (v) => 
(

            
                    ZebraTypeEnum.$safe(( v is String ? v as String :




throwArgumentMismatch(String, v)

))

)


),
className: src.getRequiredMapped(_reflection.classNamePart.oasName, (v) => 
(

            
                    ( v is String ? v as String :




throwArgumentMismatch(String, v)

)

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

  if (!src.getOrUndefined(_reflection.typePart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
     && ZebraTypeEnum.canDeserialize(v)
)
),
    unDefined: () => !_reflection.typePart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.classNamePart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
),
    unDefined: () => !_reflection.classNamePart.required,
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
Map<String, dynamic> _$ZebraSerialize(Zebra src) {
  Map<String, dynamic> initialResult = () {
    
    return src.toMap();
    
    
  }();
  return initialResult;
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

