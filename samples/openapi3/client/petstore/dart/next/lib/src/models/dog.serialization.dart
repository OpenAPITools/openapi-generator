// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'dog.dart';


//class serialization

Map<String, dynamic> _$DogToMap(Dog instance) {
  final _reflection = DogReflection.instance;
  return <String, dynamic>{
    if (instance.color.isDefined)
    _reflection.colorPart.oasName: (
            String

 v) {
      return v;
    }(instance.color.valueRequired),
    if (instance.breed.isDefined)
    _reflection.breedPart.oasName: (
            String

 v) {
      return v;
    }(instance.breed.valueRequired),
    
    _reflection.classNamePart.oasName: (
            String

 v) {
      return v;
    }(instance.className),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

Dog _$DogFromMap(Map<String, dynamic> src) {
  const _reflection = DogReflection.instance;
  final discriminatorKey = _reflection.discriminatorKey;
  final discriminatorValue = src[discriminatorKey]?.toString();
  //when we have a discriminator, we pick one model
  final modelReflection = _reflection.tryGetDiscriminatorModel(discriminatorValue);
  return Dog.$all(
    color: src.getOrUndefinedMapped(_reflection.colorPart.oasName, (v) => 
(

            
                    ( v is String ? v as String :




throwArgumentMismatch(String, v)

)

)


),
breed: src.getOrUndefinedMapped(_reflection.breedPart.oasName, (v) => 
(

            
                    ( v is String ? v as String :




throwArgumentMismatch(String, v)

)

)


),
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
    
  );
}

bool _$DogCanFromMap(Map<String, dynamic> src) {
  final _reflection = DogReflection.instance;

  if (!src.getOrUndefined(_reflection.colorPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
),
    unDefined: () => !_reflection.colorPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.breedPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
),
    unDefined: () => !_reflection.breedPart.required,
)) {
    return false;
  }
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

  return true;
}

/// Deserializes a primitive Object (num, String, List, Map).
Dog _$DogDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$DogFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$DogCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$DogCanFromMap(src);
  } else {
    final v = src;
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Map<String, dynamic> _$DogSerialize(Dog src) {
  Map<String, dynamic> initialResult = () {
    
      return _$DogToMap(src);
    
  }();
  return initialResult;
}


/*
XmlElement _$DogToXml(Dog instance) {
  final reflection = DogXmlReflection.instance;
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

Dog _$DogFromXml(XmlElement src) {
  final reflection = DogXmlReflection.instance;
  return Dog.$all(

  );
}
*/

