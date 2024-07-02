// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'new_pet.dart';


//class serialization

Map<String, dynamic> _$NewPetToMap(NewPet instance) {
  final _reflection = NewPetReflection.instance;
  return <String, dynamic>{
    if (instance.id.isDefined)
    _reflection.id.oasName: (
            int
 v) {
      return v;
    }(instance.id.valueRequired),
    if (instance.categoryInlineAllof.isDefined)
    _reflection.categoryInlineAllof.oasName: (
            NewPetCategoryInlineAllof
 v) {
      return v.serialize();
    }(instance.categoryInlineAllof.valueRequired),
    if (instance.categoryAllOfRef.isDefined)
    _reflection.categoryAllOfRef.oasName: (
            Category
 v) {
      return v.serialize();
    }(instance.categoryAllOfRef.valueRequired),
    
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
            NewPetStatusEnum
 v) {
      return v.value;
    }(instance.status.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

NewPet _$NewPetFromMap(Map<String, dynamic> src) {
  final _reflection = NewPetReflection.instance;
  return NewPet.$all(
    id: src.getOrUndefinedMapped(_reflection.id.oasName, (v) => 
(

    
            
                    v as int
            

)


),
categoryInlineAllof: src.getOrUndefinedMapped(_reflection.categoryInlineAllof.oasName, (v) => NewPetCategoryInlineAllof.deserialize
(

    
            v


)


),
categoryAllOfRef: src.getOrUndefinedMapped(_reflection.categoryAllOfRef.oasName, (v) => Category.deserialize
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

    
            
                    NewPetStatusEnum.$safe(v as String)
            

)


),
    additionalProperties: AdditionalProperties(src.except(_reflection.knownKeys).map((key, v) => MapEntry(key, 
(
v

)
))),
    
  );
}

bool _$NewPetCanFromMap(Map<String, dynamic> src) {
  final _reflection = NewPetReflection.instance;
  if (!src.getOrUndefined(_reflection.id.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is int
),
    unDefined: () => !_reflection.id.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.categoryInlineAllof.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            NewPetCategoryInlineAllof.canDeserialize(v)
            
),
    unDefined: () => !_reflection.categoryInlineAllof.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.categoryAllOfRef.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            Category.canDeserialize(v)
            
),
    unDefined: () => !_reflection.categoryAllOfRef.required,
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
NewPet _$NewPetDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$NewPetFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$NewPetCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$NewPetCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Map<String,dynamic> _$NewPetSerialize(NewPet src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$NewPetToXml(NewPet instance) {
  final reflection = NewPetXmlReflection.instance;
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

NewPet _$NewPetFromXml(XmlElement src) {
  final reflection = NewPetXmlReflection.instance;
  return NewPet.$all(

  );
}
*/

