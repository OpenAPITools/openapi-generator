// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'equilateral_triangle.dart';


//class serialization

Map<String, dynamic> _$EquilateralTriangleToMap(EquilateralTriangle instance) {
  final _reflection = EquilateralTriangleReflection.instance;
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

EquilateralTriangle _$EquilateralTriangleFromMap(Map<String, dynamic> src) {
  final _reflection = EquilateralTriangleReflection.instance;
  return EquilateralTriangle.$all(
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

bool _$EquilateralTriangleCanFromMap(Map<String, dynamic> src) {
  final _reflection = EquilateralTriangleReflection.instance;
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
EquilateralTriangle _$EquilateralTriangleDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$EquilateralTriangleFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$EquilateralTriangleCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$EquilateralTriangleCanFromMap(src);
  } else {
    final v = src;
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Object? _$EquilateralTriangleSerialize(EquilateralTriangle src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$EquilateralTriangleToXml(EquilateralTriangle instance) {
  final reflection = EquilateralTriangleXmlReflection.instance;
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

EquilateralTriangle _$EquilateralTriangleFromXml(XmlElement src) {
  final reflection = EquilateralTriangleXmlReflection.instance;
  return EquilateralTriangle.$all(

  );
}
*/

