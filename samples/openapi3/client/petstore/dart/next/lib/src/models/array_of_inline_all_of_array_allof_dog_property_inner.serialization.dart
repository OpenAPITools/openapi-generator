// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'array_of_inline_all_of_array_allof_dog_property_inner.dart';


//class serialization

Map<String, dynamic> _$ArrayOfInlineAllOfArrayAllofDogPropertyInnerToMap(ArrayOfInlineAllOfArrayAllofDogPropertyInner instance) {
  final _reflection = ArrayOfInlineAllOfArrayAllofDogPropertyInnerReflection.instance;
  return <String, dynamic>{
    if (instance.breed.isDefined)
    _reflection.breed.oasName: (
            String
 v) {
      return v;
    }(instance.breed.valueRequired),
    if (instance.color.isDefined)
    _reflection.color.oasName: (
            String
 v) {
      return v;
    }(instance.color.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

ArrayOfInlineAllOfArrayAllofDogPropertyInner _$ArrayOfInlineAllOfArrayAllofDogPropertyInnerFromMap(Map<String, dynamic> src) {
  final _reflection = ArrayOfInlineAllOfArrayAllofDogPropertyInnerReflection.instance;
  return ArrayOfInlineAllOfArrayAllofDogPropertyInner.$all(
    breed: src.getOrUndefinedMapped(_reflection.breed.oasName, (v) => 
(

    
            
                    v as String
            

)


),
color: src.getOrUndefinedMapped(_reflection.color.oasName, (v) => 
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

bool _$ArrayOfInlineAllOfArrayAllofDogPropertyInnerCanFromMap(Map<String, dynamic> src) {
  final _reflection = ArrayOfInlineAllOfArrayAllofDogPropertyInnerReflection.instance;
  if (!src.getOrUndefined(_reflection.breed.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.breed.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.color.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.color.required,
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
Object? _$ArrayOfInlineAllOfArrayAllofDogPropertyInnerSerialize(ArrayOfInlineAllOfArrayAllofDogPropertyInner src) {
  
  return src.toMap();
  
  
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

