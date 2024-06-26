// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'triangle_interface.dart';


//class serialization

Map<String, dynamic> _$TriangleInterfaceToMap(TriangleInterface instance) {
  final _reflection = TriangleInterfaceReflection.instance;
  return <String, dynamic>{
    
    _reflection.triangleType.oasName: (
            String
 v) {
      return v;
    }(instance.triangleType),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

TriangleInterface _$TriangleInterfaceFromMap(Map<String, dynamic> src) {
  final _reflection = TriangleInterfaceReflection.instance;
  return TriangleInterface.$all(
    triangleType: src.getRequiredMapped(_reflection.triangleType.oasName, (v) => 
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

bool _$TriangleInterfaceCanFromMap(Map<String, dynamic> src) {
  final _reflection = TriangleInterfaceReflection.instance;
  if (!src.getOrUndefined(_reflection.triangleType.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.triangleType.required,
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
TriangleInterface _$TriangleInterfaceDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$TriangleInterfaceFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$TriangleInterfaceCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$TriangleInterfaceCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Object? _$TriangleInterfaceSerialize(TriangleInterface src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$TriangleInterfaceToXml(TriangleInterface instance) {
  final reflection = TriangleInterfaceXmlReflection.instance;
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

TriangleInterface _$TriangleInterfaceFromXml(XmlElement src) {
  final reflection = TriangleInterfaceXmlReflection.instance;
  return TriangleInterface.$all(

  );
}
*/

