// ignore_for_file: unnecessary_cast, unused_local_variable

part of '__return.dart';


//class serialization

Map<String, dynamic> _$$ReturnToMap($Return instance) {
  final _reflection = $ReturnReflection.instance;
  return <String, dynamic>{
    if (instance.$return.isDefined)
    _reflection.$return.oasName: (
            int
 v) {
      return v;
    }(instance.$return.valueRequired),
    
    
  };
}

$Return _$$ReturnFromMap(Map<String, dynamic> src) {
  final _reflection = $ReturnReflection.instance;
  return $Return.$all(
    $return: src.getOrUndefinedMapped(_reflection.$return.oasName, (v) => 
(

    
            
                    v as int
            

)


),
    
    
  );
}

bool _$$ReturnCanFromMap(Map<String, dynamic> src) {
  final _reflection = $ReturnReflection.instance;
  if (!src.getOrUndefined(_reflection.$return.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is int
),
    unDefined: () => !_reflection.$return.required,
)) {
    return false;
  }
  
  return true;
}

/// Deserializes a primitive Object (num, String, List, Map).
$Return _$$ReturnDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$$ReturnFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$$ReturnCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$$ReturnCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Object? _$$ReturnSerialize($Return src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$$ReturnToXml($Return instance) {
  final reflection = $ReturnXmlReflection.instance;
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

$Return _$$ReturnFromXml(XmlElement src) {
  final reflection = $ReturnXmlReflection.instance;
  return $Return.$all(

  );
}
*/

