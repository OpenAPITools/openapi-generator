// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'grandparent_animal.dart';


//class serialization

Map<String, dynamic> _$GrandparentAnimalToMap(GrandparentAnimal instance) {
  final _reflection = GrandparentAnimalReflection.instance;
  return <String, dynamic>{
    
    _reflection.petType.oasName: (
            String
 v) {
      return v;
    }(instance.petType),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

GrandparentAnimal _$GrandparentAnimalFromMap(Map<String, dynamic> src) {
  final _reflection = GrandparentAnimalReflection.instance;
  return GrandparentAnimal.$all(
    petType: src.getRequiredMapped(_reflection.petType.oasName, (v) => 
(

    
            
                    v as String
            

)


),
    additionalProperties: AdditionalProperties(src.except(_reflection.knownKeys).map((key, v) => MapEntry(key, 
(
v

)
))),
    
  );
}

bool _$GrandparentAnimalCanFromMap(Map<String, dynamic> src) {
  final _reflection = GrandparentAnimalReflection.instance;
  if (!src.getOrUndefined(_reflection.petType.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.petType.required,
)) {
    return false;
  }
  if (!src.except(_reflection.knownKeys).values.every((v) => v == null ? true :
(
true
))) {
    return false;
  }
  
  return true;
}

/// Deserializes a primitive Object (num, String, List, Map).
GrandparentAnimal _$GrandparentAnimalDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$GrandparentAnimalFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$GrandparentAnimalCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$GrandparentAnimalCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Map<String,dynamic> _$GrandparentAnimalSerialize(GrandparentAnimal src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$GrandparentAnimalToXml(GrandparentAnimal instance) {
  final reflection = GrandparentAnimalXmlReflection.instance;
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

GrandparentAnimal _$GrandparentAnimalFromXml(XmlElement src) {
  final reflection = GrandparentAnimalXmlReflection.instance;
  return GrandparentAnimal.$all(

  );
}
*/

