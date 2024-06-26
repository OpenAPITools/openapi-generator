// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'pet.dart';


//class serialization

Map<String, dynamic> _$PetToMap(Pet instance) {
  final _reflection = PetReflection.instance;
  return <String, dynamic>{
    if (instance.id.isDefined)
    _reflection.id.oasName: (
            int
 v) {
      return v;
    }(instance.id.valueRequired),
    if (instance.category.isDefined)
    _reflection.category.oasName: (
            Category
 v) {
      return v.serialize();
    }(instance.category.valueRequired),
    
    _reflection.name.oasName: (
            String
 v) {
      return v;
    }(instance.name),
    
    _reflection.photoUrls.oasName: (
    List<
        
            String
>
 v) {
      return v.map((v) => v).toList();
    }(instance.photoUrls),
    if (instance.tags.isDefined)
    _reflection.tags.oasName: (
    List<
        
            Tag
>
 v) {
      return v.map((v) => v.serialize()).toList();
    }(instance.tags.valueRequired),
    if (instance.status.isDefined)
    _reflection.status.oasName: (
            PetStatusEnum
 v) {
      return v.value;
    }(instance.status.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

Pet _$PetFromMap(Map<String, dynamic> src) {
  final _reflection = PetReflection.instance;
  return Pet.$all(
    id: src.getOrUndefinedMapped(_reflection.id.oasName, (v) => 
(

    
            
                    v as int
            

)


),
category: src.getOrUndefinedMapped(_reflection.category.oasName, (v) => Category.deserialize
(

    
            v


)


),
name: src.getRequiredMapped(_reflection.name.oasName, (v) => 
(

    
            
                    v as String
            

)


),
photoUrls: src.getRequiredMapped(_reflection.photoUrls.oasName, (v) => 
(

    
            
            v as List
            

)

.map((v) => 
(

    
            
                    v as String
            

)


).toList()
),
tags: src.getOrUndefinedMapped(_reflection.tags.oasName, (v) => 
(

    
            
            v as List
            

)

.map((v) => Tag.deserialize
(

    
            v


)


).toList()
),
status: src.getOrUndefinedMapped(_reflection.status.oasName, (v) => 
(

    
            
                    PetStatusEnum.$safe(v as String)
            

)


),
    additionalProperties: AdditionalProperties(src.except(_reflection.knownKeys).map((key, v) => MapEntry(key, 
(
v

)
))),
    
  );
}

bool _$PetCanFromMap(Map<String, dynamic> src) {
  final _reflection = PetReflection.instance;
  if (!src.getOrUndefined(_reflection.id.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is int
),
    unDefined: () => !_reflection.id.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.category.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            Category.canDeserialize(v)
            
),
    unDefined: () => !_reflection.category.required,
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
if (!src.getOrUndefined(_reflection.photoUrls.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is List && v.every((v) => v == null ? false :
(

    
            
            v is String
))
),
    unDefined: () => !_reflection.photoUrls.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.tags.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is List && v.every((v) => v == null ? false :
(

    
            Tag.canDeserialize(v)
            
))
),
    unDefined: () => !_reflection.tags.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.status.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.status.required,
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
Pet _$PetDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$PetFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$PetCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$PetCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Object? _$PetSerialize(Pet src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$PetToXml(Pet instance) {
  final reflection = PetXmlReflection.instance;
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

Pet _$PetFromXml(XmlElement src) {
  final reflection = PetXmlReflection.instance;
  return Pet.$all(

  );
}
*/

