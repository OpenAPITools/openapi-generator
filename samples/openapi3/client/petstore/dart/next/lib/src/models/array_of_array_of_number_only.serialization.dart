// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'array_of_array_of_number_only.dart';


//class serialization

Map<String, dynamic> _$ArrayOfArrayOfNumberOnlyToMap(ArrayOfArrayOfNumberOnly instance) {
  final _reflection = ArrayOfArrayOfNumberOnlyReflection.instance;
  return <String, dynamic>{
    if (instance.arrayArrayNumber.isDefined)
    _reflection.arrayArrayNumberPart.oasName: (
    List<
        
    List<
        
            num
>
>
 v) {
      return v.map((v) => v.map((v) => v).toList()).toList();
    }(instance.arrayArrayNumber.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

ArrayOfArrayOfNumberOnly _$ArrayOfArrayOfNumberOnlyFromMap(Map<String, dynamic> src) {
  const _reflection = ArrayOfArrayOfNumberOnlyReflection.instance;
  return ArrayOfArrayOfNumberOnly.$all(
    arrayArrayNumber: src.getOrUndefinedMapped(_reflection.arrayArrayNumberPart.oasName, (v) => 
(

            
            v as List
            

)

.map((v) => 
(

            
            v as List
            

)

.map((v) => 
(

            
                    ( v is num ? v as num :
num.parse(v.toString())



)

)


).toList()
).toList()
),
    additionalProperties: AdditionalProperties(src.except(_reflection.knownKeys).map((key, v) => MapEntry(key, 
(
v

)
))),
    
  );
}

bool _$ArrayOfArrayOfNumberOnlyCanFromMap(Map<String, dynamic> src) {
  final _reflection = ArrayOfArrayOfNumberOnlyReflection.instance;

  if (!src.getOrUndefined(_reflection.arrayArrayNumberPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is List && v.every((v) => v == null ? false :
(

    
            
            v is List && v.every((v) => v == null ? false :
(

    
            
            (v is num
     || (num.tryParse(v.toString()) != null)
    
    
    
)
))
))
),
    unDefined: () => !_reflection.arrayArrayNumberPart.required,
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
ArrayOfArrayOfNumberOnly _$ArrayOfArrayOfNumberOnlyDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$ArrayOfArrayOfNumberOnlyFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$ArrayOfArrayOfNumberOnlyCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$ArrayOfArrayOfNumberOnlyCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Map<String, dynamic> _$ArrayOfArrayOfNumberOnlySerialize(ArrayOfArrayOfNumberOnly src) {
  Map<String, dynamic> initialResult = () {
    
    return src.toMap();
    
    
  }();
  return initialResult;
}


/*
XmlElement _$ArrayOfArrayOfNumberOnlyToXml(ArrayOfArrayOfNumberOnly instance) {
  final reflection = ArrayOfArrayOfNumberOnlyXmlReflection.instance;
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

ArrayOfArrayOfNumberOnly _$ArrayOfArrayOfNumberOnlyFromXml(XmlElement src) {
  final reflection = ArrayOfArrayOfNumberOnlyXmlReflection.instance;
  return ArrayOfArrayOfNumberOnly.$all(

  );
}
*/

