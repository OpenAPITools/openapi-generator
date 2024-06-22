// ignore_for_file: unnecessary_cast, unused_local_variable

part of '__list.dart';


//class serialization

Map<String, dynamic> _$$ListToMap($List instance) {
  final _reflection = $ListReflection.instance;
  return <String, dynamic>{
    if (instance.$123list.isDefined)
    _reflection.$123list.oasName: (
            String
 v) {
      return v;
    }(instance.$123list.valueRequired),
    
    
  };
}

$List _$$ListFromMap(Map<String, dynamic> src) {
  final _reflection = $ListReflection.instance;
  return $List.$all(
    $123list: src.getOrUndefinedMapped(_reflection.$123list.oasName, (v) => 
(

    
            
                    v as String
            

)


),
    
    
  );
}

bool _$$ListCanFromMap(Map<String, dynamic> src) {
  final _reflection = $ListReflection.instance;
  if (!src.getOrUndefined(_reflection.$123list.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.$123list.required,
)) {
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
Object? _$$ListSerialize($List src) {
  
  return src.toMap();
  
  
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

