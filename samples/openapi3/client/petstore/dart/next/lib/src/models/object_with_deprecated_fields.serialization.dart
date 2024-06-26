// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'object_with_deprecated_fields.dart';


//class serialization

Map<String, dynamic> _$ObjectWithDeprecatedFieldsToMap(ObjectWithDeprecatedFields instance) {
  final _reflection = ObjectWithDeprecatedFieldsReflection.instance;
  return <String, dynamic>{
    if (instance.uuid.isDefined)
    _reflection.uuid.oasName: (
            String
 v) {
      return v;
    }(instance.uuid.valueRequired),
    if (instance.id.isDefined)
    _reflection.id.oasName: (
            num
 v) {
      return v;
    }(instance.id.valueRequired),
    if (instance.deprecatedRef.isDefined)
    _reflection.deprecatedRef.oasName: (
            DeprecatedObject
 v) {
      return v.serialize();
    }(instance.deprecatedRef.valueRequired),
    if (instance.bars.isDefined)
    _reflection.bars.oasName: (
    List<
        
            String
>
 v) {
      return v.map((v) => v).toList();
    }(instance.bars.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

ObjectWithDeprecatedFields _$ObjectWithDeprecatedFieldsFromMap(Map<String, dynamic> src) {
  final _reflection = ObjectWithDeprecatedFieldsReflection.instance;
  return ObjectWithDeprecatedFields.$all(
    uuid: src.getOrUndefinedMapped(_reflection.uuid.oasName, (v) => 
(

    
            
                    v as String
            

)


),
id: src.getOrUndefinedMapped(_reflection.id.oasName, (v) => 
(

    
            
                    v as num
            

)


),
deprecatedRef: src.getOrUndefinedMapped(_reflection.deprecatedRef.oasName, (v) => DeprecatedObject.deserialize
(

    
            v


)


),
bars: src.getOrUndefinedMapped(_reflection.bars.oasName, (v) => 
(

    
            
            v as List
            

)

.map((v) => 
(

    
            
                    v as String
            

)


).toList()
),
    additionalProperties: AdditionalProperties(src.except(_reflection.knownKeys).map((key, v) => MapEntry(key, 
(
v

)
))),
    
  );
}

bool _$ObjectWithDeprecatedFieldsCanFromMap(Map<String, dynamic> src) {
  final _reflection = ObjectWithDeprecatedFieldsReflection.instance;
  if (!src.getOrUndefined(_reflection.uuid.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.uuid.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.id.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is num
),
    unDefined: () => !_reflection.id.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.deprecatedRef.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            DeprecatedObject.canDeserialize(v)
            
),
    unDefined: () => !_reflection.deprecatedRef.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.bars.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is List && v.every((v) => v == null ? false :
(

    
            
            v is String
))
),
    unDefined: () => !_reflection.bars.required,
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
ObjectWithDeprecatedFields _$ObjectWithDeprecatedFieldsDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$ObjectWithDeprecatedFieldsFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$ObjectWithDeprecatedFieldsCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$ObjectWithDeprecatedFieldsCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Object? _$ObjectWithDeprecatedFieldsSerialize(ObjectWithDeprecatedFields src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$ObjectWithDeprecatedFieldsToXml(ObjectWithDeprecatedFields instance) {
  final reflection = ObjectWithDeprecatedFieldsXmlReflection.instance;
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

ObjectWithDeprecatedFields _$ObjectWithDeprecatedFieldsFromXml(XmlElement src) {
  final reflection = ObjectWithDeprecatedFieldsXmlReflection.instance;
  return ObjectWithDeprecatedFields.$all(

  );
}
*/

