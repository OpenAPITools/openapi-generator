// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'array_of_inline_all_of.dart';


//class serialization

Map<String, dynamic> _$ArrayOfInlineAllOfToMap(ArrayOfInlineAllOf instance) {
  final _reflection = ArrayOfInlineAllOfReflection.instance;
  return <String, dynamic>{
    if (instance.id.isDefined)
    _reflection.idPart.oasName: (
            int

 v) {
      return v;
    }(instance.id.valueRequired),
    
    _reflection.namePart.oasName: (
            String

 v) {
      return v;
    }(instance.name),
    if (instance.arrayAllofDogProperty.isDefined)
    _reflection.arrayAllofDogPropertyPart.oasName: (
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
  const _reflection = ArrayOfInlineAllOfReflection.instance;
  return ArrayOfInlineAllOf.$all(
    id: src.getOrUndefinedMapped(_reflection.idPart.oasName, (v) => 
(

            
                    ( v is int ? v as int :
int.parse(v.toString())



)

)


),
name: src.getRequiredMapped(_reflection.namePart.oasName, (v) => 
(

            
                    ( v is String ? v as String :




throwArgumentMismatch(String, v)

)

)


),
arrayAllofDogProperty: src.getOrUndefinedMapped(_reflection.arrayAllofDogPropertyPart.oasName, (v) => 
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

  if (!src.getOrUndefined(_reflection.idPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is int
     || (int.tryParse(v.toString()) != null)
    
    
    
)
),
    unDefined: () => !_reflection.idPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.namePart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
),
    unDefined: () => !_reflection.namePart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.arrayAllofDogPropertyPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is List && v.every((v) => v == null ? false :
(

    
            ArrayOfInlineAllOfArrayAllofDogPropertyInner.canDeserialize(v)
            
))
),
    unDefined: () => !_reflection.arrayAllofDogPropertyPart.required,
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
Map<String, dynamic> _$ArrayOfInlineAllOfSerialize(ArrayOfInlineAllOf src) {
  Map<String, dynamic> initialResult = () {
    
      return _$ArrayOfInlineAllOfToMap(src);
    
  }();
  return initialResult;
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

