// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'parent_pet.dart';


//class serialization

Map<String, dynamic> _$ParentPetToMap(ParentPet instance) {
  final _reflection = ParentPetReflection.instance;
  return <String, dynamic>{
    
    _reflection.petTypePart.oasName: (
            String
 v) {
      return v;
    }(instance.petType),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

ParentPet _$ParentPetFromMap(Map<String, dynamic> src) {
  const _reflection = ParentPetReflection.instance;
  final discriminatorKey = _reflection.discriminatorKey;
  final discriminatorValue = src[discriminatorKey]?.toString();
  //when we have a discriminator, we pick one model
  final modelReflection = _reflection.tryGetDiscriminatorModel(discriminatorValue);
  return ParentPet.$all(
    petType: src.getRequiredMapped(_reflection.petTypePart.oasName, (v) => 
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

bool _$ParentPetCanFromMap(Map<String, dynamic> src) {
  final _reflection = ParentPetReflection.instance;

  if (!src.getOrUndefined(_reflection.petTypePart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
),
    unDefined: () => !_reflection.petTypePart.required,
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
ParentPet _$ParentPetDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$ParentPetFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$ParentPetCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$ParentPetCanFromMap(src);
  } else {
    final v = src;
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Map<String, dynamic> _$ParentPetSerialize(ParentPet src) {
  Map<String, dynamic> initialResult = () {
    
    return src.toMap();
    
    
  }();
  return initialResult;
}


/*
XmlElement _$ParentPetToXml(ParentPet instance) {
  final reflection = ParentPetXmlReflection.instance;
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

ParentPet _$ParentPetFromXml(XmlElement src) {
  final reflection = ParentPetXmlReflection.instance;
  return ParentPet.$all(

  );
}
*/

