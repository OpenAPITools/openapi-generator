// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'cat.dart';


//class serialization

Map<String, dynamic> _$CatToMap(Cat instance) {
  final _reflection = CatReflection.instance;
  return <String, dynamic>{
    if (instance.color.isDefined)
    _reflection.color.oasName: (
            String
 v) {
      return v;
    }(instance.color.valueRequired),
    if (instance.declawed.isDefined)
    _reflection.declawed.oasName: (
            bool
 v) {
      return v;
    }(instance.declawed.valueRequired),
    
    _reflection.className.oasName: (
            String
 v) {
      return v;
    }(instance.className),
    
    
  };
}

Cat _$CatFromMap(Map<String, dynamic> src) {
  final _reflection = CatReflection.instance;
  return Cat.$all(
    color: src.getOrUndefinedMapped(_reflection.color.oasName, (v) => 
(

    
            
                    v as String
            

)


),
declawed: src.getOrUndefinedMapped(_reflection.declawed.oasName, (v) => 
(

    
            
                    v as bool
            

)


),
className: src.getRequiredMapped(_reflection.className.oasName, (v) => 
(

    
            
                    v as String
            

)


),
    
    
  );
}

bool _$CatCanFromMap(Map<String, dynamic> src) {
  final _reflection = CatReflection.instance;
  if (!src.getOrUndefined(_reflection.color.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.color.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.declawed.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is bool
),
    unDefined: () => !_reflection.declawed.required,
)) {
    return false;
  }
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
Cat _$CatDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$CatFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$CatCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$CatCanFromMap(src);
  } else {
    final v = src;
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Object? _$CatSerialize(Cat src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$CatToXml(Cat instance) {
  final reflection = CatXmlReflection.instance;
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

Cat _$CatFromXml(XmlElement src) {
  final reflection = CatXmlReflection.instance;
  return Cat.$all(

  );
}
*/

