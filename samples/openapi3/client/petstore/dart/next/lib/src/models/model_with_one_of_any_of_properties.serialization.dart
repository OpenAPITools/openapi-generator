// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'model_with_one_of_any_of_properties.dart';


//class serialization

Map<String, dynamic> _$ModelWithOneOfAnyOfPropertiesToMap(ModelWithOneOfAnyOfProperties instance) {
  final _reflection = ModelWithOneOfAnyOfPropertiesReflection.instance;
  return <String, dynamic>{
    if (instance.oneofProp.isDefined)
    _reflection.oneofProp.oasName: (
            ArrayOneOf
 v) {
      return v.serialize();
    }(instance.oneofProp.valueRequired),
    if (instance.anyofProp.isDefined)
    _reflection.anyofProp.oasName: (
            ArrayAnyOf
 v) {
      return v.serialize();
    }(instance.anyofProp.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

ModelWithOneOfAnyOfProperties _$ModelWithOneOfAnyOfPropertiesFromMap(Map<String, dynamic> src) {
  final _reflection = ModelWithOneOfAnyOfPropertiesReflection.instance;
  return ModelWithOneOfAnyOfProperties.$all(
    oneofProp: src.getOrUndefinedMapped(_reflection.oneofProp.oasName, (v) => ArrayOneOf.deserialize
(

    
            v


)


),
anyofProp: src.getOrUndefinedMapped(_reflection.anyofProp.oasName, (v) => ArrayAnyOf.deserialize
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

bool _$ModelWithOneOfAnyOfPropertiesCanFromMap(Map<String, dynamic> src) {
  final _reflection = ModelWithOneOfAnyOfPropertiesReflection.instance;
  if (!src.getOrUndefined(_reflection.oneofProp.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            ArrayOneOf.canDeserialize(v)
            
),
    unDefined: () => !_reflection.oneofProp.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.anyofProp.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            ArrayAnyOf.canDeserialize(v)
            
),
    unDefined: () => !_reflection.anyofProp.required,
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
ModelWithOneOfAnyOfProperties _$ModelWithOneOfAnyOfPropertiesDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$ModelWithOneOfAnyOfPropertiesFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$ModelWithOneOfAnyOfPropertiesCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$ModelWithOneOfAnyOfPropertiesCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Map<String,dynamic> _$ModelWithOneOfAnyOfPropertiesSerialize(ModelWithOneOfAnyOfProperties src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$ModelWithOneOfAnyOfPropertiesToXml(ModelWithOneOfAnyOfProperties instance) {
  final reflection = ModelWithOneOfAnyOfPropertiesXmlReflection.instance;
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

ModelWithOneOfAnyOfProperties _$ModelWithOneOfAnyOfPropertiesFromXml(XmlElement src) {
  final reflection = ModelWithOneOfAnyOfPropertiesXmlReflection.instance;
  return ModelWithOneOfAnyOfProperties.$all(

  );
}
*/

