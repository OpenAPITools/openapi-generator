// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'array_of_inline_all_of.dart';


//class serialization

Map<String, dynamic> _$ArrayOfInlineAllOfToMap(ArrayOfInlineAllOf instance) {
  final _reflection = ArrayOfInlineAllOfReflection.instance;
  return <String, dynamic>{
    if (instance.id.isDefined)
    _reflection.id.oasName: (
            int
 v) {
      return v;
    }(instance.id.valueRequired),
    
    _reflection.name.oasName: (
            String
 v) {
      return v;
    }(instance.name),
    if (instance.arrayAllofDogProperty.isDefined)
    _reflection.arrayAllofDogProperty.oasName: (
    List<
        
            ArrayOfInlineAllOfArrayAllofDogPropertyInner
>
 v) {
      return v.map((v) => v.serialize()).toList();
    }(instance.arrayAllofDogProperty.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

ArrayOfInlineAllOf _$ArrayOfInlineAllOfFromMap(Map<String, dynamic> src) {
  final _reflection = ArrayOfInlineAllOfReflection.instance;
  return ArrayOfInlineAllOf.$all(
    id: src.getOrUndefinedMapped(_reflection.id.oasName, (v) => 
(

    
            
                    v as int
            

)


),
name: src.getRequiredMapped(_reflection.name.oasName, (v) => 
(

    
            
                    v as String
            

)


),
arrayAllofDogProperty: src.getOrUndefinedMapped(_reflection.arrayAllofDogProperty.oasName, (v) => 
(

    
            
            v as List
            

)

.map((v) => ArrayOfInlineAllOfArrayAllofDogPropertyInner.deserialize
(

    
            v


)


).toList()
),
    additionalProperties: AdditionalProperties(src.except(_reflection.knownKeys).map((key, v) => MapEntry(key, 
(
v

)
))),
    
  );
}

bool _$ArrayOfInlineAllOfCanFromMap(Map<String, dynamic> src) {
  final _reflection = ArrayOfInlineAllOfReflection.instance;
  if (!src.getOrUndefined(_reflection.id.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is int
),
    unDefined: () => !_reflection.id.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.name.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.name.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.arrayAllofDogProperty.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is List && v.every((v) => v == null ? false :
(

    
            ArrayOfInlineAllOfArrayAllofDogPropertyInner.canDeserialize(v)
            
))
),
    unDefined: () => !_reflection.arrayAllofDogProperty.required,
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
ArrayOfInlineAllOf _$ArrayOfInlineAllOfDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$ArrayOfInlineAllOfFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$ArrayOfInlineAllOfCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$ArrayOfInlineAllOfCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Map<String,dynamic> _$ArrayOfInlineAllOfSerialize(ArrayOfInlineAllOf src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$ArrayOfInlineAllOfToXml(ArrayOfInlineAllOf instance) {
  final reflection = ArrayOfInlineAllOfXmlReflection.instance;
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

ArrayOfInlineAllOf _$ArrayOfInlineAllOfFromXml(XmlElement src) {
  final reflection = ArrayOfInlineAllOfXmlReflection.instance;
  return ArrayOfInlineAllOf.$all(

  );
}
*/

