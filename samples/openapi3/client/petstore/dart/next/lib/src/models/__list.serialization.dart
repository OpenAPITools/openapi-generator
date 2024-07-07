// ignore_for_file: unnecessary_cast, unused_local_variable

part of '__list.dart';


//class serialization

Map<String, dynamic> _$$ListToMap($List instance) {
  final _reflection = $ListReflection.instance;
  return <String, dynamic>{
    if (instance.$123list.isDefined)
    _reflection.$123listPart.oasName: (
            String
 v) {
      return v;
    }(instance.$123list.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

$List _$$ListFromMap(Map<String, dynamic> src) {
  const _reflection = $ListReflection.instance;
  return $List.$all(
    $123list: src.getOrUndefinedMapped(_reflection.$123listPart.oasName, (v) => 
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

bool _$$ListCanFromMap(Map<String, dynamic> src) {
  final _reflection = $ListReflection.instance;

  if (!src.getOrUndefined(_reflection.$123listPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
),
    unDefined: () => !_reflection.$123listPart.required,
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
$List _$$ListDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$$ListFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$$ListCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$$ListCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Map<String, dynamic> _$$ListSerialize($List src) {
  Map<String, dynamic> initialResult = () {
    
    return src.toMap();
    
    
  }();
  return initialResult;
}


/*
XmlElement _$$ListToXml($List instance) {
  final reflection = $ListXmlReflection.instance;
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

$List _$$ListFromXml(XmlElement src) {
  final reflection = $ListXmlReflection.instance;
  return $List.$all(

  );
}
*/

