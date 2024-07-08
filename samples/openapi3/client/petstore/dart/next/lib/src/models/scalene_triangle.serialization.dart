// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'scalene_triangle.dart';


//class serialization

Map<String, dynamic> _$ScaleneTriangleToMap(ScaleneTriangle instance) {
  final _reflection = ScaleneTriangleReflection.instance;
  return <String, dynamic>{
    
    _reflection.shapeTypePart.oasName: (
            String

 v) {
      return v;
    }(instance.shapeType),
    
    _reflection.triangleTypePart.oasName: (
            String

 v) {
      return v;
    }(instance.triangleType),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

ScaleneTriangle _$ScaleneTriangleFromMap(Map<String, dynamic> src) {
  const _reflection = ScaleneTriangleReflection.instance;
  return ScaleneTriangle.$all(
    shapeType: src.getRequiredMapped(_reflection.shapeTypePart.oasName, (v) => 
(

            
                    ( v is String ? v as String :




throwArgumentMismatch(String, v)

)

)


),
triangleType: src.getRequiredMapped(_reflection.triangleTypePart.oasName, (v) => 
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

bool _$ScaleneTriangleCanFromMap(Map<String, dynamic> src) {
  final _reflection = ScaleneTriangleReflection.instance;

  if (!src.getOrUndefined(_reflection.shapeTypePart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
),
    unDefined: () => !_reflection.shapeTypePart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.triangleTypePart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
),
    unDefined: () => !_reflection.triangleTypePart.required,
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
Map<String, dynamic> _$ScaleneTriangleSerialize(ScaleneTriangle src) {
  Map<String, dynamic> initialResult = () {
    
      return _$ScaleneTriangleToMap(src);
    
  }();
  return initialResult;
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

