// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'simple_quadrilateral.dart';


//class serialization

Map<String, dynamic> _$SimpleQuadrilateralToMap(SimpleQuadrilateral instance) {
  final _reflection = SimpleQuadrilateralReflection.instance;
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

SimpleQuadrilateral _$SimpleQuadrilateralFromMap(Map<String, dynamic> src) {
  const _reflection = SimpleQuadrilateralReflection.instance;
  return SimpleQuadrilateral.$all(
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

bool _$SimpleQuadrilateralCanFromMap(Map<String, dynamic> src) {
  final _reflection = SimpleQuadrilateralReflection.instance;

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
Map<String, dynamic> _$SimpleQuadrilateralSerialize(SimpleQuadrilateral src) {
  Map<String, dynamic> initialResult = () {
    
      return _$SimpleQuadrilateralToMap(src);
    
  }();
  return initialResult;
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

