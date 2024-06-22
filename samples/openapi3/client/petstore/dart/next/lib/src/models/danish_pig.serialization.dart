// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'danish_pig.dart';


//class serialization

Map<String, dynamic> _$DanishPigToMap(DanishPig instance) {
  final _reflection = DanishPigReflection.instance;
  return <String, dynamic>{
    
    _reflection.className.oasName: (
            String
 v) {
      return v;
    }(instance.className),
    
    
  };
}

DanishPig _$DanishPigFromMap(Map<String, dynamic> src) {
  final _reflection = DanishPigReflection.instance;
  return DanishPig.$all(
    className: src.getRequiredMapped(_reflection.className.oasName, (v) => 
(

    
            
                    v as String
            

)


),
    
    
  );
}

bool _$DanishPigCanFromMap(Map<String, dynamic> src) {
  final _reflection = DanishPigReflection.instance;
  if (!src.getOrUndefined(_reflection.className.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.className.required,
)) {
    return false;
  }
  
  return true;
}

/// Deserializes a primitive Object (num, String, List, Map).
DanishPig _$DanishPigDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$DanishPigFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$DanishPigCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$DanishPigCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Object? _$DanishPigSerialize(DanishPig src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$DanishPigToXml(DanishPig instance) {
  final reflection = DanishPigXmlReflection.instance;
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

DanishPig _$DanishPigFromXml(XmlElement src) {
  final reflection = DanishPigXmlReflection.instance;
  return DanishPig.$all(

  );
}
*/

