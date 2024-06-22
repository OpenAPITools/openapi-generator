// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'apple_req.dart';


//class serialization

Map<String, dynamic> _$AppleReqToMap(AppleReq instance) {
  final _reflection = AppleReqReflection.instance;
  return <String, dynamic>{
    
    _reflection.cultivar.oasName: (
            String
 v) {
      return v;
    }(instance.cultivar),
    if (instance.mealy.isDefined)
    _reflection.mealy.oasName: (
            bool
 v) {
      return v;
    }(instance.mealy.valueRequired),
    
    
  };
}

AppleReq _$AppleReqFromMap(Map<String, dynamic> src) {
  final _reflection = AppleReqReflection.instance;
  return AppleReq.$all(
    cultivar: src.getRequiredMapped(_reflection.cultivar.oasName, (v) => 
(

    
            
                    v as String
            

)


),
mealy: src.getOrUndefinedMapped(_reflection.mealy.oasName, (v) => 
(

    
            
                    v as bool
            

)


),
    
    
  );
}

bool _$AppleReqCanFromMap(Map<String, dynamic> src) {
  final _reflection = AppleReqReflection.instance;
  if (!src.getOrUndefined(_reflection.cultivar.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.cultivar.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.mealy.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is bool
),
    unDefined: () => !_reflection.mealy.required,
)) {
    return false;
  }
  
  return true;
}

/// Deserializes a primitive Object (num, String, List, Map).
AppleReq _$AppleReqDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$AppleReqFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$AppleReqCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$AppleReqCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Object? _$AppleReqSerialize(AppleReq src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$AppleReqToXml(AppleReq instance) {
  final reflection = AppleReqXmlReflection.instance;
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

AppleReq _$AppleReqFromXml(XmlElement src) {
  final reflection = AppleReqXmlReflection.instance;
  return AppleReq.$all(

  );
}
*/

