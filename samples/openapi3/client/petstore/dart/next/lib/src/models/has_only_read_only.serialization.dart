// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'has_only_read_only.dart';


//class serialization

Map<String, dynamic> _$HasOnlyReadOnlyToMap(HasOnlyReadOnly instance) {
  final _reflection = HasOnlyReadOnlyReflection.instance;
  return <String, dynamic>{
    if (instance.bar.isDefined)
    _reflection.bar.oasName: (
            String
 v) {
      return v;
    }(instance.bar.valueRequired),
    if (instance.foo.isDefined)
    _reflection.foo.oasName: (
            String
 v) {
      return v;
    }(instance.foo.valueRequired),
    
    
  };
}

HasOnlyReadOnly _$HasOnlyReadOnlyFromMap(Map<String, dynamic> src) {
  final _reflection = HasOnlyReadOnlyReflection.instance;
  return HasOnlyReadOnly.$all(
    bar: src.getOrUndefinedMapped(_reflection.bar.oasName, (v) => 
(

    
            
                    v as String
            

)


),
foo: src.getOrUndefinedMapped(_reflection.foo.oasName, (v) => 
(

    
            
                    v as String
            

)


),
    
    
  );
}

bool _$HasOnlyReadOnlyCanFromMap(Map<String, dynamic> src) {
  final _reflection = HasOnlyReadOnlyReflection.instance;
  if (!src.getOrUndefined(_reflection.bar.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.bar.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.foo.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.foo.required,
)) {
    return false;
  }
  
  return true;
}

/// Deserializes a primitive Object (num, String, List, Map).
HasOnlyReadOnly _$HasOnlyReadOnlyDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$HasOnlyReadOnlyFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$HasOnlyReadOnlyCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$HasOnlyReadOnlyCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Object? _$HasOnlyReadOnlySerialize(HasOnlyReadOnly src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$HasOnlyReadOnlyToXml(HasOnlyReadOnly instance) {
  final reflection = HasOnlyReadOnlyXmlReflection.instance;
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

HasOnlyReadOnly _$HasOnlyReadOnlyFromXml(XmlElement src) {
  final reflection = HasOnlyReadOnlyXmlReflection.instance;
  return HasOnlyReadOnly.$all(

  );
}
*/

