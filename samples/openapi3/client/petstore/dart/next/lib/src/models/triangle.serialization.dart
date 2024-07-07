// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'triangle.dart';


//class serialization

Map<String, dynamic> _$TriangleToMap(Triangle instance) {
  final _reflection = TriangleReflection.instance;
  return <String, dynamic>{
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
    
    final v = src;
    return Triangle.$all(
      oneOf0: (v == null ? false :
(

    
            EquilateralTriangle.canDeserialize(v)
            
)) ? UndefinedWrapper(EquilateralTriangle.deserialize
(

            v

)


) : UndefinedWrapper.undefined(),      oneOf1: (v == null ? false :
(

    
            IsoscelesTriangle.canDeserialize(v)
            
)) ? UndefinedWrapper(IsoscelesTriangle.deserialize
(

            v

)


) : UndefinedWrapper.undefined(),      oneOf2: (v == null ? false :
(

    
            ScaleneTriangle.canDeserialize(v)
            
)) ? UndefinedWrapper(ScaleneTriangle.deserialize
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
Object? _$TriangleSerialize(Triangle src) {
  Object? initialResult = () {
    
    
    if (src.oneOf0.isDefined) {final v = src.oneOf0.valueRequired; return v.serialize(); }
    if (src.oneOf1.isDefined) {final v = src.oneOf1.valueRequired; return v.serialize(); }
    if (src.oneOf2.isDefined) {final v = src.oneOf2.valueRequired; return v.serialize(); }
    return null;
  }();
  if (initialResult is Map<String, Object?>) {
    return {
      ...src.additionalProperties,
      ...initialResult,
    };
  }
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

