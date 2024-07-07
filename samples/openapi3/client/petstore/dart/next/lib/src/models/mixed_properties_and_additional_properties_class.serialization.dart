// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'mixed_properties_and_additional_properties_class.dart';


//class serialization

Map<String, dynamic> _$MixedPropertiesAndAdditionalPropertiesClassToMap(MixedPropertiesAndAdditionalPropertiesClass instance) {
  final _reflection = MixedPropertiesAndAdditionalPropertiesClassReflection.instance;
  return <String, dynamic>{
    if (instance.uuid.isDefined)
    _reflection.uuidPart.oasName: (
            String
 v) {
      return v;
    }(instance.uuid.valueRequired),
    if (instance.dateTime.isDefined)
    _reflection.dateTimePart.oasName: (
            DateTime
 v) {
      return v;
    }(instance.dateTime.valueRequired),
    if (instance.map.isDefined)
    _reflection.mapPart.oasName: (
    Map<String, 
        
            Animal
>
 v) {
      return v.map((k,v) => MapEntry(k, v.serialize()));
    }(instance.map.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

MixedPropertiesAndAdditionalPropertiesClass _$MixedPropertiesAndAdditionalPropertiesClassFromMap(Map<String, dynamic> src) {
  const _reflection = MixedPropertiesAndAdditionalPropertiesClassReflection.instance;
  return MixedPropertiesAndAdditionalPropertiesClass.$all(
    uuid: src.getOrUndefinedMapped(_reflection.uuidPart.oasName, (v) => 
(

            
                    ( v is String ? v as String :




throwArgumentMismatch(String, v)

)

)


),
dateTime: src.getOrUndefinedMapped(_reflection.dateTimePart.oasName, (v) => 
(

            
                    ( v is DateTime ? v as DateTime :


v is int ? DateTime.fromMillisecondsSinceEpoch(v as int) : DateTime.parse(v.toString())

)

)


),
map: src.getOrUndefinedMapped(_reflection.mapPart.oasName, (v) => 
(

            v as Map<String, dynamic>
            
            

)
.map((k,v) => MapEntry(k, Animal.deserialize
(

            v

)


))

),
    additionalProperties: AdditionalProperties(src.except(_reflection.knownKeys).map((key, v) => MapEntry(key, 
(
v

)
))),
    
  );
}

bool _$MixedPropertiesAndAdditionalPropertiesClassCanFromMap(Map<String, dynamic> src) {
  final _reflection = MixedPropertiesAndAdditionalPropertiesClassReflection.instance;

  if (!src.getOrUndefined(_reflection.uuidPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
),
    unDefined: () => !_reflection.uuidPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.dateTimePart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is DateTime
    
    
     || (v is int || DateTime.tryParse(v.toString()) != null)
    
)
),
    unDefined: () => !_reflection.dateTimePart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.mapPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            v is Map<String, dynamic> && v.values.every((v) => v == null ? false :
(

    
            Animal.canDeserialize(v)
            
))
            
),
    unDefined: () => !_reflection.mapPart.required,
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
Map<String, dynamic> _$MixedPropertiesAndAdditionalPropertiesClassSerialize(MixedPropertiesAndAdditionalPropertiesClass src) {
  Map<String, dynamic> initialResult = () {
    
    return src.toMap();
    
    
  }();
  return initialResult;
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

