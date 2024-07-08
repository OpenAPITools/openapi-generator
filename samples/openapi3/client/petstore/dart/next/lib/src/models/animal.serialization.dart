// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'animal.dart';


//class serialization

Map<String, dynamic> _$AnimalToMap(Animal instance) {
  final _reflection = AnimalReflection.instance;
  return <String, dynamic>{
    
    _reflection.classNamePart.oasName: (
            String

 v) {
      return v;
    }(instance.className),
    if (instance.color.isDefined)
    _reflection.colorPart.oasName: (
            String

 v) {
      return v;
    }(instance.color.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

Animal _$AnimalFromMap(Map<String, dynamic> src) {
  const _reflection = AnimalReflection.instance;
  final discriminatorKey = _reflection.discriminatorKey;
  final discriminatorValue = src[discriminatorKey]?.toString();
  //when we have a discriminator, we pick one model
  final modelReflection = _reflection.tryGetDiscriminatorModel(discriminatorValue);
  return Animal.$all(
    className: src.getRequiredMapped(_reflection.classNamePart.oasName, (v) => 
(

            
                    ( v is String ? v as String :




throwArgumentMismatch(String, v)

)

)


),
color: src.getOrUndefinedMapped(_reflection.colorPart.oasName, (v) => 
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

bool _$AnimalCanFromMap(Map<String, dynamic> src) {
  final _reflection = AnimalReflection.instance;

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
  if (!src.except(_reflection.knownKeys).values.every((v) => v == null ? true :
(
true
))) {
    return false;
  }

  return true;
}

/// Deserializes a primitive Object (num, String, List, Map).
Animal _$AnimalDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$AnimalFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$AnimalCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$AnimalCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Map<String, dynamic> _$AnimalSerialize(Animal src) {
  Map<String, dynamic> initialResult = () {
    
      return _$AnimalToMap(src);
    
  }();
  return initialResult;
}


/*
XmlElement _$AnimalToXml(Animal instance) {
  final reflection = AnimalXmlReflection.instance;
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

Animal _$AnimalFromXml(XmlElement src) {
  final reflection = AnimalXmlReflection.instance;
  return Animal.$all(

  );
}
*/

