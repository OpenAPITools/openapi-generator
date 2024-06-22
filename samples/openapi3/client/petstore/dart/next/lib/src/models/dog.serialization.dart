// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'dog.dart';


//class serialization

Map<String, dynamic> _$DogToMap(Dog instance) {
  final _reflection = DogReflection.instance;
  return <String, dynamic>{
    if (instance.color.isDefined)
    _reflection.color.oasName: (
            String
 v) {
      return v;
    }(instance.color.valueRequired),
    if (instance.breed.isDefined)
    _reflection.breed.oasName: (
            String
 v) {
      return v;
    }(instance.breed.valueRequired),
    
    _reflection.className.oasName: (
            String
 v) {
      return v;
    }(instance.className),
    
    
  };
}

Dog _$DogFromMap(Map<String, dynamic> src) {
  final _reflection = DogReflection.instance;
  return Dog.$all(
    color: src.getOrUndefinedMapped(_reflection.color.oasName, (v) => 
(

    
            
                    v as String
            

)


),
breed: src.getOrUndefinedMapped(_reflection.breed.oasName, (v) => 
(

    
            
                    v as String
            

)


),
className: src.getRequiredMapped(_reflection.className.oasName, (v) => 
(

    
            
                    v as String
            

)


),
    
    
  );
}

bool _$DogCanFromMap(Map<String, dynamic> src) {
  final _reflection = DogReflection.instance;
  if (!src.getOrUndefined(_reflection.color.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.color.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.breed.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.breed.required,
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
Dog _$DogDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$DogFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$DogCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$DogCanFromMap(src);
  } else {
    final v = src;
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Object? _$DogSerialize(Dog src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$DogToXml(Dog instance) {
  final reflection = DogXmlReflection.instance;
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

Dog _$DogFromXml(XmlElement src) {
  final reflection = DogXmlReflection.instance;
  return Dog.$all(

  );
}
*/

