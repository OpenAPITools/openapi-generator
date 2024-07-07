// ignore_for_file: unnecessary_cast, unused_local_variable

part of '__200_response.dart';


//class serialization

Map<String, dynamic> _$$200ResponseToMap($200Response instance) {
  final _reflection = $200ResponseReflection.instance;
  return <String, dynamic>{
    if (instance.name.isDefined)
    _reflection.namePart.oasName: (
            int
 v) {
      return v;
    }(instance.name.valueRequired),
    if (instance.propertyClass.isDefined)
    _reflection.propertyClassPart.oasName: (
            String
 v) {
      return v;
    }(instance.propertyClass.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

$200Response _$$200ResponseFromMap(Map<String, dynamic> src) {
  const _reflection = $200ResponseReflection.instance;
  return $200Response.$all(
    name: src.getOrUndefinedMapped(_reflection.namePart.oasName, (v) => 
(

            
                    ( v is int ? v as int :
int.parse(v.toString())



)

)


),
propertyClass: src.getOrUndefinedMapped(_reflection.propertyClassPart.oasName, (v) => 
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

bool _$$200ResponseCanFromMap(Map<String, dynamic> src) {
  final _reflection = $200ResponseReflection.instance;

  if (!src.getOrUndefined(_reflection.namePart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is int
     || (int.tryParse(v.toString()) != null)
    
    
    
)
),
    unDefined: () => !_reflection.namePart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.propertyClassPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
),
    unDefined: () => !_reflection.propertyClassPart.required,
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
$200Response _$$200ResponseDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$$200ResponseFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$$200ResponseCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$$200ResponseCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Map<String, dynamic> _$$200ResponseSerialize($200Response src) {
  Map<String, dynamic> initialResult = () {
    
    return src.toMap();
    
    
  }();
  return initialResult;
}


/*
XmlElement _$$200ResponseToXml($200Response instance) {
  final reflection = $200ResponseXmlReflection.instance;
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

$200Response _$$200ResponseFromXml(XmlElement src) {
  final reflection = $200ResponseXmlReflection.instance;
  return $200Response.$all(

  );
}
*/

