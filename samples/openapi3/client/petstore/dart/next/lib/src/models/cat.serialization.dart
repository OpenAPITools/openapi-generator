// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'cat.dart';


//class serialization

Map<String, dynamic> _$CatToMap(Cat instance) {
  final _reflection = CatReflection.instance;
  return <String, dynamic>{
    if (instance.color.isDefined)
    _reflection.colorPart.oasName: (
            String
 v) {
      return v;
    }(instance.color.valueRequired),
    if (instance.declawed.isDefined)
    _reflection.declawedPart.oasName: (
            bool
 v) {
      return v;
    }(instance.declawed.valueRequired),
    
    _reflection.classNamePart.oasName: (
            String
 v) {
      return v;
    }(instance.className),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

Cat _$CatFromMap(Map<String, dynamic> src) {
  const _reflection = CatReflection.instance;
  final discriminatorKey = _reflection.discriminatorKey;
  final discriminatorValue = src[discriminatorKey]?.toString();
  //when we have a discriminator, we pick one model
  final modelReflection = _reflection.tryGetDiscriminatorModel(discriminatorValue);
  return Cat.$all(
    color: src.getOrUndefinedMapped(_reflection.colorPart.oasName, (v) => 
(

            
                    ( v is String ? v as String :




throwArgumentMismatch(String, v)

)

)


),
declawed: src.getOrUndefinedMapped(_reflection.declawedPart.oasName, (v) => 
(

            
                    ( v is bool ? v as bool :

bool.parse(v.toString())


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

bool _$CatCanFromMap(Map<String, dynamic> src) {
  final _reflection = CatReflection.instance;

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
if (!src.getOrUndefined(_reflection.declawedPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is bool
    
     || (bool.tryParse(v.toString()) != null)
    
    
)
),
    unDefined: () => !_reflection.declawedPart.required,
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
Cat _$CatDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$CatFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$CatCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$CatCanFromMap(src);
  } else {
    final v = src;
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Map<String, dynamic> _$CatSerialize(Cat src) {
  Map<String, dynamic> initialResult = () {
    
    return src.toMap();
    
    
  }();
  return initialResult;
}


/*
XmlElement _$CatToXml(Cat instance) {
  final reflection = CatXmlReflection.instance;
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

Cat _$CatFromXml(XmlElement src) {
  final reflection = CatXmlReflection.instance;
  return Cat.$all(

  );
}
*/

