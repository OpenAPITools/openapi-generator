// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'free_form_object_test_class.dart';


//class serialization

Map<String, dynamic> _$FreeFormObjectTestClassToMap(FreeFormObjectTestClass instance) {
  final _reflection = FreeFormObjectTestClassReflection.instance;
  return <String, dynamic>{
    if (instance.name.isDefined)
    _reflection.name.oasName: (
            String
 v) {
      return v;
    }(instance.name.valueRequired),
    if (instance.properties.isDefined)
    _reflection.properties.oasName: (
            FreeFormObjectTestClassProperties
 v) {
      return v.serialize();
    }(instance.properties.valueRequired),
    
    
  };
}

FreeFormObjectTestClass _$FreeFormObjectTestClassFromMap(Map<String, dynamic> src) {
  final _reflection = FreeFormObjectTestClassReflection.instance;
  return FreeFormObjectTestClass.$all(
    name: src.getOrUndefinedMapped(_reflection.name.oasName, (v) => 
(

    
            
                    v as String
            

)


),
properties: src.getOrUndefinedMapped(_reflection.properties.oasName, (v) => FreeFormObjectTestClassProperties.deserialize
(

    
            v


)


),
    
    
  );
}

bool _$FreeFormObjectTestClassCanFromMap(Map<String, dynamic> src) {
  final _reflection = FreeFormObjectTestClassReflection.instance;
  if (!src.getOrUndefined(_reflection.name.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.name.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.properties.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            FreeFormObjectTestClassProperties.canDeserialize(v)
            
),
    unDefined: () => !_reflection.properties.required,
)) {
    return false;
  }
  
  return true;
}

/// Deserializes a primitive Object (num, String, List, Map).
FreeFormObjectTestClass _$FreeFormObjectTestClassDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$FreeFormObjectTestClassFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$FreeFormObjectTestClassCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$FreeFormObjectTestClassCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Object? _$FreeFormObjectTestClassSerialize(FreeFormObjectTestClass src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$FreeFormObjectTestClassToXml(FreeFormObjectTestClass instance) {
  final reflection = FreeFormObjectTestClassXmlReflection.instance;
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

FreeFormObjectTestClass _$FreeFormObjectTestClassFromXml(XmlElement src) {
  final reflection = FreeFormObjectTestClassXmlReflection.instance;
  return FreeFormObjectTestClass.$all(

  );
}
*/

