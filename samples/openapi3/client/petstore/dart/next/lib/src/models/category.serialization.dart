// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'category.dart';


//class serialization

Map<String, dynamic> _$CategoryToMap(Category instance) {
  final _reflection = CategoryReflection.instance;
  return <String, dynamic>{
    if (instance.id.isDefined)
    _reflection.id.oasName: (
            int
 v) {
      return v;
    }(instance.id.valueRequired),
    
    _reflection.name.oasName: (
            String
 v) {
      return v;
    }(instance.name),
    
    
  };
}

Category _$CategoryFromMap(Map<String, dynamic> src) {
  final _reflection = CategoryReflection.instance;
  return Category.$all(
    id: src.getOrUndefinedMapped(_reflection.id.oasName, (v) => 
(

    
            
                    v as int
            

)


),
name: src.getRequiredMapped(_reflection.name.oasName, (v) => 
(

    
            
                    v as String
            

)


),
    
    
  );
}

bool _$CategoryCanFromMap(Map<String, dynamic> src) {
  final _reflection = CategoryReflection.instance;
  if (!src.getOrUndefined(_reflection.id.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is int
),
    unDefined: () => !_reflection.id.required,
)) {
    return false;
  }
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
Category _$CategoryDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$CategoryFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$CategoryCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$CategoryCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Object? _$CategorySerialize(Category src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$CategoryToXml(Category instance) {
  final reflection = CategoryXmlReflection.instance;
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

Category _$CategoryFromXml(XmlElement src) {
  final reflection = CategoryXmlReflection.instance;
  return Category.$all(

  );
}
*/

