// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'shape_or_null.dart';


//class serialization

Map<String, dynamic> _$ShapeOrNullToMap(ShapeOrNull instance) {
  final _reflection = ShapeOrNullReflection.instance;
  return <String, dynamic>{
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
    if (instance.oneOf0.isDefined) ...instance.oneOf0.valueRequired.toMap(),
    
    if (instance.oneOf1.isDefined) ...instance.oneOf1.valueRequired.toMap(),
    
  };
}

ShapeOrNull _$ShapeOrNullFromMap(Map<String, dynamic> src) {
  const _reflection = ShapeOrNullReflection.instance;
  final discriminatorKey = _reflection.discriminatorKey;
  final discriminatorValue = src[discriminatorKey]?.toString();
  //when we have a discriminator, we pick one model
  final modelReflection = _reflection.tryGetDiscriminatorModel(discriminatorValue);
  return ShapeOrNull.$all(
        additionalProperties: AdditionalProperties(src.except(_reflection.knownKeys).map((key, v) => MapEntry(key, 
(
v

)
))),
    
    oneOf0: modelReflection is ClassReflection<Triangle> ? UndefinedWrapper(modelReflection.deserializeFunction(src)) : UndefinedWrapper.undefined(),
    oneOf1: modelReflection is ClassReflection<Quadrilateral> ? UndefinedWrapper(modelReflection.deserializeFunction(src)) : UndefinedWrapper.undefined(),
  );
}

bool _$ShapeOrNullCanFromMap(Map<String, dynamic> src) {
  final _reflection = ShapeOrNullReflection.instance;

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
ShapeOrNull _$ShapeOrNullDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$ShapeOrNullFromMap(src);
  } else {
    
    final v = src;
    return ShapeOrNull.$all(
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
bool _$ShapeOrNullCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$ShapeOrNullCanFromMap(src);
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
Map<String, dynamic> _$ShapeOrNullSerialize(ShapeOrNull src) {
  Map<String, dynamic> initialResult = () {
    
        return _$ShapeOrNullToMap(src);
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
XmlElement _$ShapeOrNullToXml(ShapeOrNull instance) {
  final reflection = ShapeOrNullXmlReflection.instance;
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

ShapeOrNull _$ShapeOrNullFromXml(XmlElement src) {
  final reflection = ShapeOrNullXmlReflection.instance;
  return ShapeOrNull.$all(

  );
}
*/

