// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'simple_quadrilateral.dart';


//class serialization

Map<String, dynamic> _$SimpleQuadrilateralToMap(SimpleQuadrilateral instance) {
  final _reflection = SimpleQuadrilateralReflection.instance;
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

SimpleQuadrilateral _$SimpleQuadrilateralFromMap(Map<String, dynamic> src) {
  final _reflection = SimpleQuadrilateralReflection.instance;
  return SimpleQuadrilateral.$all(
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

bool _$SimpleQuadrilateralCanFromMap(Map<String, dynamic> src) {
  final _reflection = SimpleQuadrilateralReflection.instance;
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
SimpleQuadrilateral _$SimpleQuadrilateralDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$SimpleQuadrilateralFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$SimpleQuadrilateralCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$SimpleQuadrilateralCanFromMap(src);
  } else {
    final v = src;
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Object? _$SimpleQuadrilateralSerialize(SimpleQuadrilateral src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$SimpleQuadrilateralToXml(SimpleQuadrilateral instance) {
  final reflection = SimpleQuadrilateralXmlReflection.instance;
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

SimpleQuadrilateral _$SimpleQuadrilateralFromXml(XmlElement src) {
  final reflection = SimpleQuadrilateralXmlReflection.instance;
  return SimpleQuadrilateral.$all(

  );
}
*/

