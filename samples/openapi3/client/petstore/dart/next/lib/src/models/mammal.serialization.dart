// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'mammal.dart';


//class serialization

Map<String, dynamic> _$MammalToMap(Mammal instance) {
  final _reflection = MammalReflection.instance;
  return <String, dynamic>{
    
    _reflection.classNamePart.oasName: (
            String

 v) {
      return v;
    }(instance.className),
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
    className: src.getRequiredMapped(_reflection.classNamePart.oasName, (v) => 
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
    
    oneOf0: modelReflection is ClassReflection<Whale> ? UndefinedWrapper(modelReflection.deserializeFunction(src)) : UndefinedWrapper.undefined(),
    oneOf1: modelReflection is ClassReflection<Zebra> ? UndefinedWrapper(modelReflection.deserializeFunction(src)) : UndefinedWrapper.undefined(),
    oneOf2: modelReflection is ClassReflection<Pig> ? UndefinedWrapper(modelReflection.deserializeFunction(src)) : UndefinedWrapper.undefined(),
  );
}

bool _$MammalCanFromMap(Map<String, dynamic> src) {
  final _reflection = MammalReflection.instance;

  if (!src.getOrUndefined(_reflection.classNamePart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
),
    unDefined: () => !_reflection.classNamePart.required,
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
        
    throw UnimplementedError();
    
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
Map<String, dynamic> _$MammalSerialize(Mammal src) {
  Map<String, dynamic> initialResult = () {
    
      return _$MammalToMap(src);
    
  }();
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

