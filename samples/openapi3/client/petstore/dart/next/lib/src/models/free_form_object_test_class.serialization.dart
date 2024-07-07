// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'free_form_object_test_class.dart';


//class serialization

Map<String, dynamic> _$FreeFormObjectTestClassToMap(FreeFormObjectTestClass instance) {
  final _reflection = FreeFormObjectTestClassReflection.instance;
  return <String, dynamic>{
    if (instance.name.isDefined)
    _reflection.namePart.oasName: (
            String
 v) {
      return v;
    }(instance.name.valueRequired),
    if (instance.properties.isDefined)
    _reflection.propertiesPart.oasName: (
            FreeFormObjectTestClassProperties
 v) {
      return v.serialize();
    }(instance.properties.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

FreeFormObjectTestClass _$FreeFormObjectTestClassFromMap(Map<String, dynamic> src) {
  const _reflection = FreeFormObjectTestClassReflection.instance;
  return FreeFormObjectTestClass.$all(
    name: src.getOrUndefinedMapped(_reflection.namePart.oasName, (v) => 
(

            
                    ( v is String ? v as String :




throwArgumentMismatch(String, v)

)

)


),
properties: src.getOrUndefinedMapped(_reflection.propertiesPart.oasName, (v) => FreeFormObjectTestClassProperties.deserialize
(

            v

)


),
    additionalProperties: AdditionalProperties(src.except(_reflection.knownKeys).map((key, v) => MapEntry(key, 
(
v

)
))),
    
  );
}

bool _$FreeFormObjectTestClassCanFromMap(Map<String, dynamic> src) {
  final _reflection = FreeFormObjectTestClassReflection.instance;

  if (!src.getOrUndefined(_reflection.namePart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
),
    unDefined: () => !_reflection.namePart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.propertiesPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            FreeFormObjectTestClassProperties.canDeserialize(v)
            
),
    unDefined: () => !_reflection.propertiesPart.required,
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
Map<String, dynamic> _$FreeFormObjectTestClassSerialize(FreeFormObjectTestClass src) {
  Map<String, dynamic> initialResult = () {
    
    return src.toMap();
    
    
  }();
  return initialResult;
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

