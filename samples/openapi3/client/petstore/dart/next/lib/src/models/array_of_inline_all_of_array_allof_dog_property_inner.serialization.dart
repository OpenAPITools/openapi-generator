// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'array_of_inline_all_of_array_allof_dog_property_inner.dart';


//class serialization

Map<String, dynamic> _$ArrayOfInlineAllOfArrayAllofDogPropertyInnerToMap(ArrayOfInlineAllOfArrayAllofDogPropertyInner instance) {
  final _reflection = ArrayOfInlineAllOfArrayAllofDogPropertyInnerReflection.instance;
  return <String, dynamic>{
    if (instance.breed.isDefined)
    _reflection.breedPart.oasName: (
            String
 v) {
      return v;
    }(instance.breed.valueRequired),
    if (instance.color.isDefined)
    _reflection.colorPart.oasName: (
            String
 v) {
      return v;
    }(instance.color.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

ArrayOfInlineAllOfArrayAllofDogPropertyInner _$ArrayOfInlineAllOfArrayAllofDogPropertyInnerFromMap(Map<String, dynamic> src) {
  const _reflection = ArrayOfInlineAllOfArrayAllofDogPropertyInnerReflection.instance;
  return ArrayOfInlineAllOfArrayAllofDogPropertyInner.$all(
    breed: src.getOrUndefinedMapped(_reflection.breedPart.oasName, (v) => 
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

bool _$ArrayOfInlineAllOfArrayAllofDogPropertyInnerCanFromMap(Map<String, dynamic> src) {
  final _reflection = ArrayOfInlineAllOfArrayAllofDogPropertyInnerReflection.instance;

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
ArrayOfInlineAllOfArrayAllofDogPropertyInner _$ArrayOfInlineAllOfArrayAllofDogPropertyInnerDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$ArrayOfInlineAllOfArrayAllofDogPropertyInnerFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$ArrayOfInlineAllOfArrayAllofDogPropertyInnerCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$ArrayOfInlineAllOfArrayAllofDogPropertyInnerCanFromMap(src);
  } else {
    final v = src;
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Map<String, dynamic> _$ArrayOfInlineAllOfArrayAllofDogPropertyInnerSerialize(ArrayOfInlineAllOfArrayAllofDogPropertyInner src) {
  Map<String, dynamic> initialResult = () {
    
    return src.toMap();
    
    
  }();
  return initialResult;
}


/*
XmlElement _$ArrayOfInlineAllOfArrayAllofDogPropertyInnerToXml(ArrayOfInlineAllOfArrayAllofDogPropertyInner instance) {
  final reflection = ArrayOfInlineAllOfArrayAllofDogPropertyInnerXmlReflection.instance;
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

ArrayOfInlineAllOfArrayAllofDogPropertyInner _$ArrayOfInlineAllOfArrayAllofDogPropertyInnerFromXml(XmlElement src) {
  final reflection = ArrayOfInlineAllOfArrayAllofDogPropertyInnerXmlReflection.instance;
  return ArrayOfInlineAllOfArrayAllofDogPropertyInner.$all(

  );
}
*/

