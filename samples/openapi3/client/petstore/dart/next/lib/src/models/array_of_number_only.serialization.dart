// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'array_of_number_only.dart';


//class serialization

Map<String, dynamic> _$ArrayOfNumberOnlyToMap(ArrayOfNumberOnly instance) {
  final _reflection = ArrayOfNumberOnlyReflection.instance;
  return <String, dynamic>{
    if (instance.arrayNumber.isDefined)
    _reflection.arrayNumber.oasName: (
    List<
        
            num
>
 v) {
      return v.map((v) => v).toList();
    }(instance.arrayNumber.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

ArrayOfNumberOnly _$ArrayOfNumberOnlyFromMap(Map<String, dynamic> src) {
  final _reflection = ArrayOfNumberOnlyReflection.instance;
  return ArrayOfNumberOnly.$all(
    arrayNumber: src.getOrUndefinedMapped(_reflection.arrayNumber.oasName, (v) => 
(

    
            
            v as List
            

)

.map((v) => 
(

    
            
                    v as num
            

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

bool _$ArrayOfNumberOnlyCanFromMap(Map<String, dynamic> src) {
  final _reflection = ArrayOfNumberOnlyReflection.instance;
  if (!src.getOrUndefined(_reflection.arrayNumber.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is List && v.every((v) => v == null ? false :
(

    
            
            v is num
))
),
    unDefined: () => !_reflection.arrayNumber.required,
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
ArrayOfNumberOnly _$ArrayOfNumberOnlyDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$ArrayOfNumberOnlyFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$ArrayOfNumberOnlyCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$ArrayOfNumberOnlyCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Map<String,dynamic> _$ArrayOfNumberOnlySerialize(ArrayOfNumberOnly src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$ArrayOfNumberOnlyToXml(ArrayOfNumberOnly instance) {
  final reflection = ArrayOfNumberOnlyXmlReflection.instance;
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

ArrayOfNumberOnly _$ArrayOfNumberOnlyFromXml(XmlElement src) {
  final reflection = ArrayOfNumberOnlyXmlReflection.instance;
  return ArrayOfNumberOnly.$all(

  );
}
*/

