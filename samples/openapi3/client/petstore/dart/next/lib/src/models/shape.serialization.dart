// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'shape.dart';


//class serialization

Map<String, dynamic> _$ShapeToMap(Shape instance) {
  final _reflection = ShapeReflection.instance;
  return <String, dynamic>{
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
    if (instance.oneOf0.isDefined) ...instance.oneOf0.valueRequired.toMap(),
    
    if (instance.oneOf1.isDefined) ...instance.oneOf1.valueRequired.toMap(),
    
  };
}

Shape _$ShapeFromMap(Map<String, dynamic> src) {
  const _reflection = ShapeReflection.instance;
  final discriminatorKey = _reflection.discriminatorKey;
  final discriminatorValue = src[discriminatorKey]?.toString();
  //when we have a discriminator, we pick one model
  final modelReflection = _reflection.tryGetDiscriminatorModel(discriminatorValue);
  return Shape.$all(
        additionalProperties: AdditionalProperties(src.except(_reflection.knownKeys).map((key, v) => MapEntry(key, 
(
v

)
))),
    
    oneOf0: modelReflection is ClassReflection<Triangle> ? UndefinedWrapper(modelReflection.deserializeFunction(src)) : UndefinedWrapper.undefined(),
    oneOf1: modelReflection is ClassReflection<Quadrilateral> ? UndefinedWrapper(modelReflection.deserializeFunction(src)) : UndefinedWrapper.undefined(),
  );
}

bool _$ShapeCanFromMap(Map<String, dynamic> src) {
  final _reflection = ShapeReflection.instance;

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
    () => Triangle.canDeserialize(src),
      () => Quadrilateral.canDeserialize(src),
  ];
  final validOneOfs = oneOfs.where((x) => x()).take(2).length;
  if (validOneOfs == 0 || validOneOfs > 1) {
    return false;
  }
  
  return true;
}

/// Deserializes a primitive Object (num, String, List, Map).
Shape _$ShapeDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$ShapeFromMap(src);
  } else {
    
    final v = src;
    return Shape.$all(
      oneOf0: (v == null ? false :
(

    
            Triangle.canDeserialize(v)
            
)) ? UndefinedWrapper(Triangle.deserialize
(

            v

)


) : UndefinedWrapper.undefined(),      oneOf1: (v == null ? false :
(

    
            Quadrilateral.canDeserialize(v)
            
)) ? UndefinedWrapper(Quadrilateral.deserialize
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
bool _$ShapeCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$ShapeCanFromMap(src);
  } else {
    final v = src;
    final oneOfs = [
      () => v == null ? false :
(

    
            Triangle.canDeserialize(v)
            
),
      () => v == null ? false :
(

    
            Quadrilateral.canDeserialize(v)
            
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
Map<String, dynamic> _$ShapeSerialize(Shape src) {
  Map<String, dynamic> initialResult = () {
    
        return _$ShapeToMap(src);
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
XmlElement _$ShapeToXml(Shape instance) {
  final reflection = ShapeXmlReflection.instance;
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

Shape _$ShapeFromXml(XmlElement src) {
  final reflection = ShapeXmlReflection.instance;
  return Shape.$all(

  );
}
*/

