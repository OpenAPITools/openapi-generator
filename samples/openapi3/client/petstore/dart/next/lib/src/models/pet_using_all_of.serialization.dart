// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'pet_using_all_of.dart';


//class serialization

Map<String, dynamic> _$PetUsingAllOfToMap(PetUsingAllOf instance) {
  final _reflection = PetUsingAllOfReflection.instance;
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
            PetUsingAllOfStatusEnum
 v) {
      return v.value;
    }(instance.status.valueRequired),
    
    
  };
}

PetUsingAllOf _$PetUsingAllOfFromMap(Map<String, dynamic> src) {
  final _reflection = PetUsingAllOfReflection.instance;
  return PetUsingAllOf.$all(
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

    
            
                    PetUsingAllOfStatusEnum.$safe(v as String)
            

)


),
    
    
  );
}

bool _$PetUsingAllOfCanFromMap(Map<String, dynamic> src) {
  final _reflection = PetUsingAllOfReflection.instance;
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
PetUsingAllOf _$PetUsingAllOfDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$PetUsingAllOfFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$PetUsingAllOfCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$PetUsingAllOfCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Object? _$PetUsingAllOfSerialize(PetUsingAllOf src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$PetUsingAllOfToXml(PetUsingAllOf instance) {
  final reflection = PetUsingAllOfXmlReflection.instance;
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

PetUsingAllOf _$PetUsingAllOfFromXml(XmlElement src) {
  final reflection = PetUsingAllOfXmlReflection.instance;
  return PetUsingAllOf.$all(

  );
}
*/

