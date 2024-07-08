// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'triangle.dart';


//class serialization

Map<String, dynamic> _$TriangleToMap(Triangle instance) {
  final _reflection = TriangleReflection.instance;
  return <String, dynamic>{
    
    _reflection.triangleTypePart.oasName: (
            String

 v) {
      return v;
    }(instance.triangleType),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
    if (instance.oneOf0.isDefined) ...instance.oneOf0.valueRequired.toMap(),
    
    if (instance.oneOf1.isDefined) ...instance.oneOf1.valueRequired.toMap(),
    
    if (instance.oneOf2.isDefined) ...instance.oneOf2.valueRequired.toMap(),
    
  };
}

Triangle _$TriangleFromMap(Map<String, dynamic> src) {
  const _reflection = TriangleReflection.instance;
  final discriminatorKey = _reflection.discriminatorKey;
  final discriminatorValue = src[discriminatorKey]?.toString();
  //when we have a discriminator, we pick one model
  final modelReflection = _reflection.tryGetDiscriminatorModel(discriminatorValue);
  return Triangle.$all(
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
    
    oneOf0: modelReflection is ClassReflection<EquilateralTriangle> ? UndefinedWrapper(modelReflection.deserializeFunction(src)) : UndefinedWrapper.undefined(),
    oneOf1: modelReflection is ClassReflection<IsoscelesTriangle> ? UndefinedWrapper(modelReflection.deserializeFunction(src)) : UndefinedWrapper.undefined(),
    oneOf2: modelReflection is ClassReflection<ScaleneTriangle> ? UndefinedWrapper(modelReflection.deserializeFunction(src)) : UndefinedWrapper.undefined(),
  );
}

bool _$TriangleCanFromMap(Map<String, dynamic> src) {
  final _reflection = TriangleReflection.instance;

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


  final discriminatorKey = _reflection.discriminatorKey;
  final discriminatorValue = src[discriminatorKey]?.toString();
  //when we have a discriminator, we pick one model
  final modelReflection = _reflection.tryGetDiscriminatorModel(discriminatorValue);
  if (modelReflection != null) {
    // a discriminator is defined AND it exists in the src.
    return modelReflection.canDeserializeFunction(src);
  }
  final oneOfs = [
    () => EquilateralTriangle.canDeserialize(src),
      () => IsoscelesTriangle.canDeserialize(src),
      () => ScaleneTriangle.canDeserialize(src),
  ];
  final validOneOfs = oneOfs.where((x) => x()).take(2).length;
  if (validOneOfs == 0 || validOneOfs > 1) {
    return false;
  }
  
  return true;
}

/// Deserializes a primitive Object (num, String, List, Map).
Triangle _$TriangleDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$TriangleFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$TriangleCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$TriangleCanFromMap(src);
  } else {
    final v = src;
    final oneOfs = [
      () => v == null ? false :
(

    
            EquilateralTriangle.canDeserialize(v)
            
),
      () => v == null ? false :
(

    
            IsoscelesTriangle.canDeserialize(v)
            
),
      () => v == null ? false :
(

    
            ScaleneTriangle.canDeserialize(v)
            
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
Map<String, dynamic> _$TriangleSerialize(Triangle src) {
  Map<String, dynamic> initialResult = () {
    
      return _$TriangleToMap(src);
    
  }();
  return initialResult;
}


/*
XmlElement _$TriangleToXml(Triangle instance) {
  final reflection = TriangleXmlReflection.instance;
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

Triangle _$TriangleFromXml(XmlElement src) {
  final reflection = TriangleXmlReflection.instance;
  return Triangle.$all(

  );
}
*/

