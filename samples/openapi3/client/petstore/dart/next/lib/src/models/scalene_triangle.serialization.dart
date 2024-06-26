// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'scalene_triangle.dart';


//class serialization

Map<String, dynamic> _$ScaleneTriangleToMap(ScaleneTriangle instance) {
  final _reflection = ScaleneTriangleReflection.instance;
  return <String, dynamic>{
    
    _reflection.shapeType.oasName: (
            String
 v) {
      return v;
    }(instance.shapeType),
    
    _reflection.triangleType.oasName: (
            String
 v) {
      return v;
    }(instance.triangleType),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

ScaleneTriangle _$ScaleneTriangleFromMap(Map<String, dynamic> src) {
  final _reflection = ScaleneTriangleReflection.instance;
  return ScaleneTriangle.$all(
    shapeType: src.getRequiredMapped(_reflection.shapeType.oasName, (v) => 
(

    
            
                    v as String
            

)


),
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

bool _$ScaleneTriangleCanFromMap(Map<String, dynamic> src) {
  final _reflection = ScaleneTriangleReflection.instance;
  if (!src.getOrUndefined(_reflection.shapeType.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.shapeType.required,
)) {
    return false;
  }
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
ScaleneTriangle _$ScaleneTriangleDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$ScaleneTriangleFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$ScaleneTriangleCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$ScaleneTriangleCanFromMap(src);
  } else {
    final v = src;
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Object? _$ScaleneTriangleSerialize(ScaleneTriangle src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$ScaleneTriangleToXml(ScaleneTriangle instance) {
  final reflection = ScaleneTriangleXmlReflection.instance;
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

ScaleneTriangle _$ScaleneTriangleFromXml(XmlElement src) {
  final reflection = ScaleneTriangleXmlReflection.instance;
  return ScaleneTriangle.$all(

  );
}
*/

