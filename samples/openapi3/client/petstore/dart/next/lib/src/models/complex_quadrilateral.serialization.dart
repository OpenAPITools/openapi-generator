// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'complex_quadrilateral.dart';


//class serialization

Map<String, dynamic> _$ComplexQuadrilateralToMap(ComplexQuadrilateral instance) {
  final _reflection = ComplexQuadrilateralReflection.instance;
  return <String, dynamic>{
    
    _reflection.quadrilateralTypePart.oasName: (
            String
 v) {
      return v;
    }(instance.quadrilateralType),
    
    _reflection.shapeTypePart.oasName: (
            String
 v) {
      return v;
    }(instance.shapeType),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

ComplexQuadrilateral _$ComplexQuadrilateralFromMap(Map<String, dynamic> src) {
  const _reflection = ComplexQuadrilateralReflection.instance;
  return ComplexQuadrilateral.$all(
    quadrilateralType: src.getRequiredMapped(_reflection.quadrilateralTypePart.oasName, (v) => 
(

            
                    ( v is String ? v as String :




throwArgumentMismatch(String, v)

)

)


),
shapeType: src.getRequiredMapped(_reflection.shapeTypePart.oasName, (v) => 
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

bool _$ComplexQuadrilateralCanFromMap(Map<String, dynamic> src) {
  final _reflection = ComplexQuadrilateralReflection.instance;

  if (!src.getOrUndefined(_reflection.quadrilateralTypePart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
),
    unDefined: () => !_reflection.quadrilateralTypePart.required,
)) {
    return false;
  }
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
  if (!src.except(_reflection.knownKeys).values.every((v) => v == null ? true :
(
true
))) {
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
Map<String, dynamic> _$ComplexQuadrilateralSerialize(ComplexQuadrilateral src) {
  Map<String, dynamic> initialResult = () {
    
    return src.toMap();
    
    
  }();
  return initialResult;
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

