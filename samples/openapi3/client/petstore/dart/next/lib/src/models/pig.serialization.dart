// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'pig.dart';


//class serialization

Map<String, dynamic> _$PigToMap(Pig instance) {
  final _reflection = PigReflection.instance;
  return <String, dynamic>{
    
    _reflection.classNamePart.oasName: (
            String

 v) {
      return v;
    }(instance.className),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
    if (instance.oneOf0.isDefined) ...instance.oneOf0.valueRequired.toMap(),
    
    if (instance.oneOf1.isDefined) ...instance.oneOf1.valueRequired.toMap(),
    
  };
}

Pig _$PigFromMap(Map<String, dynamic> src) {
  const _reflection = PigReflection.instance;
  final discriminatorKey = _reflection.discriminatorKey;
  final discriminatorValue = src[discriminatorKey]?.toString();
  //when we have a discriminator, we pick one model
  final modelReflection = _reflection.tryGetDiscriminatorModel(discriminatorValue);
  return Pig.$all(
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
    
    oneOf0: modelReflection is ClassReflection<BasquePig> ? UndefinedWrapper(modelReflection.deserializeFunction(src)) : UndefinedWrapper.undefined(),
    oneOf1: modelReflection is ClassReflection<DanishPig> ? UndefinedWrapper(modelReflection.deserializeFunction(src)) : UndefinedWrapper.undefined(),
  );
}

bool _$PigCanFromMap(Map<String, dynamic> src) {
  final _reflection = PigReflection.instance;

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
    () => BasquePig.canDeserialize(src),
      () => DanishPig.canDeserialize(src),
  ];
  final validOneOfs = oneOfs.where((x) => x()).take(2).length;
  if (validOneOfs == 0 || validOneOfs > 1) {
    return false;
  }
  
  return true;
}

/// Deserializes a primitive Object (num, String, List, Map).
Pig _$PigDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$PigFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$PigCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$PigCanFromMap(src);
  } else {
    final v = src;
    final oneOfs = [
      () => v == null ? false :
(

    
            BasquePig.canDeserialize(v)
            
),
      () => v == null ? false :
(

    
            DanishPig.canDeserialize(v)
            
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
Map<String, dynamic> _$PigSerialize(Pig src) {
  Map<String, dynamic> initialResult = () {
    
      return _$PigToMap(src);
    
  }();
  return initialResult;
}


/*
XmlElement _$PigToXml(Pig instance) {
  final reflection = PigXmlReflection.instance;
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

Pig _$PigFromXml(XmlElement src) {
  final reflection = PigXmlReflection.instance;
  return Pig.$all(

  );
}
*/

