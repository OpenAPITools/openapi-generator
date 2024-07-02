// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'class_model.dart';


//class serialization

Map<String, dynamic> _$ClassModelToMap(ClassModel instance) {
  final _reflection = ClassModelReflection.instance;
  return <String, dynamic>{
    if (instance.propertyClass.isDefined)
    _reflection.propertyClass.oasName: (
            String
 v) {
      return v;
    }(instance.propertyClass.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

ClassModel _$ClassModelFromMap(Map<String, dynamic> src) {
  final _reflection = ClassModelReflection.instance;
  return ClassModel.$all(
    propertyClass: src.getOrUndefinedMapped(_reflection.propertyClass.oasName, (v) => 
(

    
            
                    v as String
            

)


),
    additionalProperties: AdditionalProperties(src.except(_reflection.knownKeys).map((key, v) => MapEntry(key, 
(
v

)
))),
    
  );
}

bool _$ClassModelCanFromMap(Map<String, dynamic> src) {
  final _reflection = ClassModelReflection.instance;
  if (!src.getOrUndefined(_reflection.propertyClass.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.propertyClass.required,
)) {
    return false;
  }
  if (!src.except(_reflection.knownKeys).values.every((v) => v == null ? true :
(
true
))) {
    return false;
  }
  
  return true;
}

/// Deserializes a primitive Object (num, String, List, Map).
ClassModel _$ClassModelDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$ClassModelFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$ClassModelCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$ClassModelCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Map<String,dynamic> _$ClassModelSerialize(ClassModel src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$ClassModelToXml(ClassModel instance) {
  final reflection = ClassModelXmlReflection.instance;
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

ClassModel _$ClassModelFromXml(XmlElement src) {
  final reflection = ClassModelXmlReflection.instance;
  return ClassModel.$all(

  );
}
*/

