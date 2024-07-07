// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'mammal.dart';


//class serialization

Map<String, dynamic> _$MammalToMap(Mammal instance) {
  final _reflection = MammalReflection.instance;
  return <String, dynamic>{
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
    if (instance.oneOf0.isDefined) ...instance.oneOf0.valueRequired.toMap(),
    
    if (instance.oneOf1.isDefined) ...instance.oneOf1.valueRequired.toMap(),
    
    if (instance.oneOf2.isDefined) ...instance.oneOf2.valueRequired.toMap(),
    
  };
}

Mammal _$MammalFromMap(Map<String, dynamic> src) {
  const _reflection = MammalReflection.instance;
  final discriminatorKey = _reflection.discriminatorKey;
  final discriminatorValue = src[discriminatorKey]?.toString();
  //when we have a discriminator, we pick one model
  final modelReflection = _reflection.tryGetDiscriminatorModel(discriminatorValue);
  return Mammal.$all(
        additionalProperties: AdditionalProperties(src.except(_reflection.knownKeys).map((key, v) => MapEntry(key, 
(
v

)
))),
    
    oneOf0: modelReflection is ClassReflection<Whale> ? UndefinedWrapper(modelReflection.deserializeFunction(src)) : UndefinedWrapper.undefined(),
    oneOf1: modelReflection is ClassReflection<Zebra> ? UndefinedWrapper(modelReflection.deserializeFunction(src)) : UndefinedWrapper.undefined(),
    oneOf2: modelReflection is ClassReflection<Pig> ? UndefinedWrapper(modelReflection.deserializeFunction(src)) : UndefinedWrapper.undefined(),
  );
}

bool _$MammalCanFromMap(Map<String, dynamic> src) {
  final _reflection = MammalReflection.instance;

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
    () => Whale.canDeserialize(src),
      () => Zebra.canDeserialize(src),
      () => Pig.canDeserialize(src),
  ];
  final validOneOfs = oneOfs.where((x) => x()).take(2).length;
  if (validOneOfs == 0 || validOneOfs > 1) {
    return false;
  }
  
  return true;
}

/// Deserializes a primitive Object (num, String, List, Map).
Mammal _$MammalDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$MammalFromMap(src);
  } else {
    
    final v = src;
    return Mammal.$all(
      oneOf0: (v == null ? false :
(

    
            Whale.canDeserialize(v)
            
)) ? UndefinedWrapper(Whale.deserialize
(

            v

)


) : UndefinedWrapper.undefined(),      oneOf1: (v == null ? false :
(

    
            Zebra.canDeserialize(v)
            
)) ? UndefinedWrapper(Zebra.deserialize
(

            v

)


) : UndefinedWrapper.undefined(),      oneOf2: (v == null ? false :
(

    
            Pig.canDeserialize(v)
            
)) ? UndefinedWrapper(Pig.deserialize
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
bool _$MammalCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$MammalCanFromMap(src);
  } else {
    final v = src;
    final oneOfs = [
      () => v == null ? false :
(

    
            Whale.canDeserialize(v)
            
),
      () => v == null ? false :
(

    
            Zebra.canDeserialize(v)
            
),
      () => v == null ? false :
(

    
            Pig.canDeserialize(v)
            
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
Object? _$MammalSerialize(Mammal src) {
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
XmlElement _$MammalToXml(Mammal instance) {
  final reflection = MammalXmlReflection.instance;
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

Mammal _$MammalFromXml(XmlElement src) {
  final reflection = MammalXmlReflection.instance;
  return Mammal.$all(

  );
}
*/

