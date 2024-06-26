// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'quadrilateral.dart';


//class serialization

Map<String, dynamic> _$QuadrilateralToMap(Quadrilateral instance) {
  final _reflection = QuadrilateralReflection.instance;
  return <String, dynamic>{
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
    if (instance.oneOf0.isDefined) ...instance.oneOf0.valueRequired.toMap(),
    
    if (instance.oneOf1.isDefined) ...instance.oneOf1.valueRequired.toMap(),
    
  };
}

Quadrilateral _$QuadrilateralFromMap(Map<String, dynamic> src) {
  final _reflection = QuadrilateralReflection.instance;
  return Quadrilateral.$all(
        additionalProperties: AdditionalProperties(src.except(_reflection.knownKeys).map((key, v) => MapEntry(key, 
(
v

)
))),
    
    oneOf0: SimpleQuadrilateral.canDeserialize(src) ? UndefinedWrapper(SimpleQuadrilateral.deserialize(src)) :  UndefinedWrapper.undefined(),
    oneOf1: ComplexQuadrilateral.canDeserialize(src) ? UndefinedWrapper(ComplexQuadrilateral.deserialize(src)) :  UndefinedWrapper.undefined(),
  );
}

bool _$QuadrilateralCanFromMap(Map<String, dynamic> src) {
  final _reflection = QuadrilateralReflection.instance;
    if (!src.except(_reflection.knownKeys).values.every((v) => v == null ? true :
(
true
))) {
    return false;
  }
  
  final oneOfs = [
    () => SimpleQuadrilateral.canDeserialize(src),
  
    () => ComplexQuadrilateral.canDeserialize(src),
  ];
  final validOneOfs = oneOfs.where((x) => x()).take(2).length;
  if (validOneOfs == 0 || validOneOfs > 1) {
    return false;
  }
  
  return true;
}

/// Deserializes a primitive Object (num, String, List, Map).
Quadrilateral _$QuadrilateralDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$QuadrilateralFromMap(src);
  } else {
    
    final v = src;
    return Quadrilateral.$all(
      oneOf0: (v == null ? false :
(

    
            SimpleQuadrilateral.canDeserialize(v)
            
)) ? UndefinedWrapper(SimpleQuadrilateral.deserialize
(

    
            v


)


) : UndefinedWrapper.undefined(),      oneOf1: (v == null ? false :
(

    
            ComplexQuadrilateral.canDeserialize(v)
            
)) ? UndefinedWrapper(ComplexQuadrilateral.deserialize
(

    
            v


)


) : UndefinedWrapper.undefined(),
      // Additional Properties only make sense if the src is a Map<String, dynamic>
      additionalProperties: AdditionalProperties(),
    );
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$QuadrilateralCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$QuadrilateralCanFromMap(src);
  } else {
    final v = src;
    final oneOfs = [
      () => v == null ? false :
(

    
            SimpleQuadrilateral.canDeserialize(v)
            
),
      () => v == null ? false :
(

    
            ComplexQuadrilateral.canDeserialize(v)
            
),
    ];
    final validOneOfs = oneOfs.where((x) => x()).take(2).length;
    if (validOneOfs == 1) {
      return true;
    }
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Object? _$QuadrilateralSerialize(Quadrilateral src) {
  
  
  if (src.oneOf0.isDefined) {final v = src.oneOf0.valueRequired; return v.serialize(); }
  if (src.oneOf1.isDefined) {final v = src.oneOf1.valueRequired; return v.serialize(); }
  return null;
}


/*
XmlElement _$QuadrilateralToXml(Quadrilateral instance) {
  final reflection = QuadrilateralXmlReflection.instance;
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

Quadrilateral _$QuadrilateralFromXml(XmlElement src) {
  final reflection = QuadrilateralXmlReflection.instance;
  return Quadrilateral.$all(

  );
}
*/

