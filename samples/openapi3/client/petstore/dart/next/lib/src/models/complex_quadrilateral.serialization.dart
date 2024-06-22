// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'complex_quadrilateral.dart';


//class serialization

Map<String, dynamic> _$ComplexQuadrilateralToMap(ComplexQuadrilateral instance) {
  final _reflection = ComplexQuadrilateralReflection.instance;
  return <String, dynamic>{
    
    _reflection.quadrilateralType.oasName: (
            String
 v) {
      return v;
    }(instance.quadrilateralType),
    
    _reflection.shapeType.oasName: (
            String
 v) {
      return v;
    }(instance.shapeType),
    
    
  };
}

ComplexQuadrilateral _$ComplexQuadrilateralFromMap(Map<String, dynamic> src) {
  final _reflection = ComplexQuadrilateralReflection.instance;
  return ComplexQuadrilateral.$all(
    quadrilateralType: src.getRequiredMapped(_reflection.quadrilateralType.oasName, (v) => 
(

    
            
                    v as String
            

)


),
shapeType: src.getRequiredMapped(_reflection.shapeType.oasName, (v) => 
(

    
            
                    v as String
            

)


),
    
    
  );
}

bool _$ComplexQuadrilateralCanFromMap(Map<String, dynamic> src) {
  final _reflection = ComplexQuadrilateralReflection.instance;
  if (!src.getOrUndefined(_reflection.quadrilateralType.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.quadrilateralType.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.shapeType.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.shapeType.required,
)) {
    return false;
  }
  
  return true;
}

/// Deserializes a primitive Object (num, String, List, Map).
ComplexQuadrilateral _$ComplexQuadrilateralDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$ComplexQuadrilateralFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$ComplexQuadrilateralCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$ComplexQuadrilateralCanFromMap(src);
  } else {
    final v = src;
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Object? _$ComplexQuadrilateralSerialize(ComplexQuadrilateral src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$ComplexQuadrilateralToXml(ComplexQuadrilateral instance) {
  final reflection = ComplexQuadrilateralXmlReflection.instance;
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

ComplexQuadrilateral _$ComplexQuadrilateralFromXml(XmlElement src) {
  final reflection = ComplexQuadrilateralXmlReflection.instance;
  return ComplexQuadrilateral.$all(

  );
}
*/

