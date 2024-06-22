// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'deprecated_object.dart';


//class serialization

Map<String, dynamic> _$DeprecatedObjectToMap(DeprecatedObject instance) {
  final _reflection = DeprecatedObjectReflection.instance;
  return <String, dynamic>{
    if (instance.name.isDefined)
    _reflection.name.oasName: (
            String
 v) {
      return v;
    }(instance.name.valueRequired),
    
    
  };
}

DeprecatedObject _$DeprecatedObjectFromMap(Map<String, dynamic> src) {
  final _reflection = DeprecatedObjectReflection.instance;
  return DeprecatedObject.$all(
    name: src.getOrUndefinedMapped(_reflection.name.oasName, (v) => 
(

    
            
                    v as String
            

)


),
    
    
  );
}

bool _$DeprecatedObjectCanFromMap(Map<String, dynamic> src) {
  final _reflection = DeprecatedObjectReflection.instance;
  if (!src.getOrUndefined(_reflection.name.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.name.required,
)) {
    return false;
  }
  
  return true;
}

/// Deserializes a primitive Object (num, String, List, Map).
DeprecatedObject _$DeprecatedObjectDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$DeprecatedObjectFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$DeprecatedObjectCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$DeprecatedObjectCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Object? _$DeprecatedObjectSerialize(DeprecatedObject src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$DeprecatedObjectToXml(DeprecatedObject instance) {
  final reflection = DeprecatedObjectXmlReflection.instance;
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

DeprecatedObject _$DeprecatedObjectFromXml(XmlElement src) {
  final reflection = DeprecatedObjectXmlReflection.instance;
  return DeprecatedObject.$all(

  );
}
*/

