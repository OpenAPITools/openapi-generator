// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'pet_with_required_tags.dart';


//class serialization

Map<String, dynamic> _$PetWithRequiredTagsToMap(PetWithRequiredTags instance) {
  final _reflection = PetWithRequiredTagsReflection.instance;
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
    
    _reflection.tags.oasName: (
    List<
        
            Tag
>
 v) {
      return v.map((v) => v.serialize()).toList();
    }(instance.tags),
    if (instance.status.isDefined)
    _reflection.status.oasName: (
            PetWithRequiredTagsStatusEnum
 v) {
      return v.value;
    }(instance.status.valueRequired),
    
    
  };
}

PetWithRequiredTags _$PetWithRequiredTagsFromMap(Map<String, dynamic> src) {
  final _reflection = PetWithRequiredTagsReflection.instance;
  return PetWithRequiredTags.$all(
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
tags: src.getRequiredMapped(_reflection.tags.oasName, (v) => 
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

    
            
                    PetWithRequiredTagsStatusEnum.$safe(v as String)
            

)


),
    
    
  );
}

bool _$PetWithRequiredTagsCanFromMap(Map<String, dynamic> src) {
  final _reflection = PetWithRequiredTagsReflection.instance;
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
  
  return true;
}

/// Deserializes a primitive Object (num, String, List, Map).
PetWithRequiredTags _$PetWithRequiredTagsDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$PetWithRequiredTagsFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$PetWithRequiredTagsCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$PetWithRequiredTagsCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Object? _$PetWithRequiredTagsSerialize(PetWithRequiredTags src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$PetWithRequiredTagsToXml(PetWithRequiredTags instance) {
  final reflection = PetWithRequiredTagsXmlReflection.instance;
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

PetWithRequiredTags _$PetWithRequiredTagsFromXml(XmlElement src) {
  final reflection = PetWithRequiredTagsXmlReflection.instance;
  return PetWithRequiredTags.$all(

  );
}
*/

