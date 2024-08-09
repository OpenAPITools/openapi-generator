// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'quadrilateral.dart';


//class serialization

Map<String, dynamic> _$QuadrilateralToMap(Quadrilateral instance) {
  final _reflection = QuadrilateralReflection.instance;
  return <String, dynamic>{
    
    _reflection.quadrilateralTypePart.oasName: (
            String

 v) {
      return v;
    }(instance.quadrilateralType),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
    if (instance.oneOf0.isDefined) ...instance.oneOf0.valueRequired.toMap(),
    
    if (instance.oneOf1.isDefined) ...instance.oneOf1.valueRequired.toMap(),
    
  };
}

Quadrilateral _$QuadrilateralFromMap(Map<String, dynamic> src) {
  const _reflection = QuadrilateralReflection.instance;
  final discriminatorKey = _reflection.discriminatorKey;
  final discriminatorValue = src[discriminatorKey]?.toString();
  //when we have a discriminator, we pick one model
  final modelReflection = _reflection.tryGetDiscriminatorModel(discriminatorValue);
  return Quadrilateral.$all(
    quadrilateralType: src.getRequiredMapped(_reflection.quadrilateralTypePart.oasName, (v) => 
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
    
    oneOf0: modelReflection is ClassReflection<SimpleQuadrilateral> ? UndefinedWrapper(modelReflection.deserializeFunction(src)) : UndefinedWrapper.undefined(),
    oneOf1: modelReflection is ClassReflection<ComplexQuadrilateral> ? UndefinedWrapper(modelReflection.deserializeFunction(src)) : UndefinedWrapper.undefined(),
  );
}

bool _$QuadrilateralCanFromMap(Map<String, dynamic> src) {
  final _reflection = QuadrilateralReflection.instance;

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
  if (!src.except(_reflection.knownKeys).values.every((v) => v == null ? true :
(
true
))) {
    return false;
  }


  final discriminatorKey = _reflection.discriminatorKey;
  final discriminatorValue = src[discriminatorKey]?.toString();
  //when we have a discriminator, we pick one model
  final modelReflection = _reflection.tryGetDiscriminatorModel(discriminatorValue);
  if (modelReflection != null) {
    // a discriminator is defined AND it exists in the src.
    return modelReflection.canDeserializeFunction(src);
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
        
    throw UnimplementedError();
    
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
Map<String, dynamic> _$QuadrilateralSerialize(Quadrilateral src) {
  Map<String, dynamic> initialResult = () {
    
      return _$QuadrilateralToMap(src);
    
  }();
  return initialResult;
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

