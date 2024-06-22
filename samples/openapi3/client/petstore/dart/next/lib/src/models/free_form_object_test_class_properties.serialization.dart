// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'free_form_object_test_class_properties.dart';


//class serialization

Map<String, dynamic> _$FreeFormObjectTestClassPropertiesToMap(FreeFormObjectTestClassProperties instance) {
  final _reflection = FreeFormObjectTestClassPropertiesReflection.instance;
  return <String, dynamic>{
    
    
  };
}

FreeFormObjectTestClassProperties _$FreeFormObjectTestClassPropertiesFromMap(Map<String, dynamic> src) {
  final _reflection = FreeFormObjectTestClassPropertiesReflection.instance;
  return FreeFormObjectTestClassProperties.$all(
        
    
    oneOf0:  UndefinedWrapper.undefined(),
    oneOf1:  UndefinedWrapper.undefined(),
  );
}

bool _$FreeFormObjectTestClassPropertiesCanFromMap(Map<String, dynamic> src) {
  final _reflection = FreeFormObjectTestClassPropertiesReflection.instance;
    
  final oneOfs = [
  
  ];
  final validOneOfs = oneOfs.where((x) => x()).take(2).length;
  if (validOneOfs == 0 || validOneOfs > 1) {
    return false;
  }
  
  return true;
}

/// Deserializes a primitive Object (num, String, List, Map).
FreeFormObjectTestClassProperties _$FreeFormObjectTestClassPropertiesDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$FreeFormObjectTestClassPropertiesFromMap(src);
  } else {
    
    final v = src;
    return FreeFormObjectTestClassProperties.$all(
      oneOf0: (v == null ? false :
(

    
            
            v is String
)) ? UndefinedWrapper(
(

    
            
                    v as String
            

)


) : UndefinedWrapper.undefined(),      oneOf1: (v == null ? false :
(

    v is Map<String, dynamic>
)) ? UndefinedWrapper(
(

    v as Map<String, dynamic>

)
.map((k,v) => MapEntry(k, 
(
v

)
))

) : UndefinedWrapper.undefined(),
    );
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$FreeFormObjectTestClassPropertiesCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$FreeFormObjectTestClassPropertiesCanFromMap(src);
  } else {
    final v = src;
    final oneOfs = [
      () => v == null ? false :
(

    
            
            v is String
),
      () => v == null ? false :
(

    v is Map<String, dynamic>
),
    ];
    final validOneOfs = oneOfs.where((x) => x()).take(2).length;
    if (validOneOfs == 1) {
      return true;
    }
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Object? _$FreeFormObjectTestClassPropertiesSerialize(FreeFormObjectTestClassProperties src) {
  
  
  if (src.oneOf0.isDefined) {final v = src.oneOf0.valueRequired; return v; }
  if (src.oneOf1.isDefined) {final v = src.oneOf1.valueRequired; return v; }
  return null;
}


/*
XmlElement _$FreeFormObjectTestClassPropertiesToXml(FreeFormObjectTestClassProperties instance) {
  final reflection = FreeFormObjectTestClassPropertiesXmlReflection.instance;
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

FreeFormObjectTestClassProperties _$FreeFormObjectTestClassPropertiesFromXml(XmlElement src) {
  final reflection = FreeFormObjectTestClassPropertiesXmlReflection.instance;
  return FreeFormObjectTestClassProperties.$all(

  );
}
*/

