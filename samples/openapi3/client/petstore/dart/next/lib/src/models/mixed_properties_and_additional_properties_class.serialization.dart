// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'mixed_properties_and_additional_properties_class.dart';


//class serialization

Map<String, dynamic> _$MixedPropertiesAndAdditionalPropertiesClassToMap(MixedPropertiesAndAdditionalPropertiesClass instance) {
  final _reflection = MixedPropertiesAndAdditionalPropertiesClassReflection.instance;
  return <String, dynamic>{
    if (instance.uuid.isDefined)
    _reflection.uuid.oasName: (
            String
 v) {
      return v;
    }(instance.uuid.valueRequired),
    if (instance.dateTime.isDefined)
    _reflection.dateTime.oasName: (
            DateTime
 v) {
      return v;
    }(instance.dateTime.valueRequired),
    if (instance.map.isDefined)
    _reflection.map.oasName: (
    Map<String, 
        
            Animal
>
 v) {
      return v.map((k,v) => MapEntry(k, v.serialize()));
    }(instance.map.valueRequired),
    
    
  };
}

MixedPropertiesAndAdditionalPropertiesClass _$MixedPropertiesAndAdditionalPropertiesClassFromMap(Map<String, dynamic> src) {
  final _reflection = MixedPropertiesAndAdditionalPropertiesClassReflection.instance;
  return MixedPropertiesAndAdditionalPropertiesClass.$all(
    uuid: src.getOrUndefinedMapped(_reflection.uuid.oasName, (v) => 
(

    
            
                    v as String
            

)


),
dateTime: src.getOrUndefinedMapped(_reflection.dateTime.oasName, (v) => 
(

    
            
                    v as DateTime
            

)


),
map: src.getOrUndefinedMapped(_reflection.map.oasName, (v) => 
(

    
            v as Map<String, dynamic>
            
            

)
.map((k,v) => MapEntry(k, Animal.deserialize
(

    
            v


)


))

),
    
    
  );
}

bool _$MixedPropertiesAndAdditionalPropertiesClassCanFromMap(Map<String, dynamic> src) {
  final _reflection = MixedPropertiesAndAdditionalPropertiesClassReflection.instance;
  if (!src.getOrUndefined(_reflection.uuid.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.uuid.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.dateTime.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is DateTime
),
    unDefined: () => !_reflection.dateTime.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.map.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            v is Map<String, dynamic> && v.values.every((v) => v == null ? false :
(

    
            Animal.canDeserialize(v)
            
))
            
),
    unDefined: () => !_reflection.map.required,
)) {
    return false;
  }
  
  return true;
}

/// Deserializes a primitive Object (num, String, List, Map).
MixedPropertiesAndAdditionalPropertiesClass _$MixedPropertiesAndAdditionalPropertiesClassDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$MixedPropertiesAndAdditionalPropertiesClassFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$MixedPropertiesAndAdditionalPropertiesClassCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$MixedPropertiesAndAdditionalPropertiesClassCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Object? _$MixedPropertiesAndAdditionalPropertiesClassSerialize(MixedPropertiesAndAdditionalPropertiesClass src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$MixedPropertiesAndAdditionalPropertiesClassToXml(MixedPropertiesAndAdditionalPropertiesClass instance) {
  final reflection = MixedPropertiesAndAdditionalPropertiesClassXmlReflection.instance;
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

MixedPropertiesAndAdditionalPropertiesClass _$MixedPropertiesAndAdditionalPropertiesClassFromXml(XmlElement src) {
  final reflection = MixedPropertiesAndAdditionalPropertiesClassXmlReflection.instance;
  return MixedPropertiesAndAdditionalPropertiesClass.$all(

  );
}
*/

