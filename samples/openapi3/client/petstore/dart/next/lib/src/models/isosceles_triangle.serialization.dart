// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'isosceles_triangle.dart';


//class serialization

Map<String, dynamic> _$IsoscelesTriangleToMap(IsoscelesTriangle instance) {
  final _reflection = IsoscelesTriangleReflection.instance;
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
    
    
  };
}

IsoscelesTriangle _$IsoscelesTriangleFromMap(Map<String, dynamic> src) {
  const _reflection = IsoscelesTriangleReflection.instance;
  return IsoscelesTriangle.$all(
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
    
    
  );
}

bool _$IsoscelesTriangleCanFromMap(Map<String, dynamic> src) {
  final _reflection = IsoscelesTriangleReflection.instance;

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



  return true;
}

/// Deserializes a primitive Object (num, String, List, Map).
IsoscelesTriangle _$IsoscelesTriangleDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$IsoscelesTriangleFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$IsoscelesTriangleCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$IsoscelesTriangleCanFromMap(src);
  } else {
    final v = src;
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Map<String, dynamic> _$IsoscelesTriangleSerialize(IsoscelesTriangle src) {
  Map<String, dynamic> initialResult = () {
    
      return _$IsoscelesTriangleToMap(src);
    
  }();
  return initialResult;
}


/*
XmlElement _$IsoscelesTriangleToXml(IsoscelesTriangle instance) {
  final reflection = IsoscelesTriangleXmlReflection.instance;
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

IsoscelesTriangle _$IsoscelesTriangleFromXml(XmlElement src) {
  final reflection = IsoscelesTriangleXmlReflection.instance;
  return IsoscelesTriangle.$all(

  );
}
*/

