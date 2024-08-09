// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'new_pet.dart';


//class serialization

Map<String, dynamic> _$NewPetToMap(NewPet instance) {
  final _reflection = NewPetReflection.instance;
  return <String, dynamic>{
    if (instance.id.isDefined)
    _reflection.idPart.oasName: (
            int

 v) {
      return v;
    }(instance.id.valueRequired),
    if (instance.categoryInlineAllof.isDefined)
    _reflection.categoryInlineAllofPart.oasName: (
            NewPetCategoryInlineAllof

 v) {
      return v.serialize();
    }(instance.categoryInlineAllof.valueRequired),
    if (instance.categoryAllOfRef.isDefined)
    _reflection.categoryAllOfRefPart.oasName: (
            Category

 v) {
      return v.serialize();
    }(instance.categoryAllOfRef.valueRequired),
    
    _reflection.namePart.oasName: (
            String

 v) {
      return v;
    }(instance.name),
    
    _reflection.photoUrlsPart.oasName: (
    List<
        
            String

>

 v) {
      return v.map((v) => v).toList();
    }(instance.photoUrls),
    if (instance.tags.isDefined)
    _reflection.tagsPart.oasName: (
    List<
        
            Tag

>

 v) {
      return v.map((v) => v.serialize()).toList();
    }(instance.tags.valueRequired),
    if (instance.status.isDefined)
    _reflection.statusPart.oasName: (
            NewPetStatusEnum

 v) {
      return v.value;
    }(instance.status.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

NewPet _$NewPetFromMap(Map<String, dynamic> src) {
  const _reflection = NewPetReflection.instance;
  return NewPet.$all(
    id: src.getOrUndefinedMapped(_reflection.idPart.oasName, (v) => 
(

            
                    ( v is int ? v as int :
int.parse(v.toString())



)

)


),
categoryInlineAllof: src.getOrUndefinedMapped(_reflection.categoryInlineAllofPart.oasName, (v) => NewPetCategoryInlineAllof.deserialize
(

            v

)


),
categoryAllOfRef: src.getOrUndefinedMapped(_reflection.categoryAllOfRefPart.oasName, (v) => Category.deserialize
(

            v

)


),
name: src.getRequiredMapped(_reflection.namePart.oasName, (v) => 
(

            
                    ( v is String ? v as String :




throwArgumentMismatch(String, v)

)

)


),
photoUrls: src.getRequiredMapped(_reflection.photoUrlsPart.oasName, (v) => 
(

            
            v as List
            

)

.map((v) => 
(

            
                    ( v is String ? v as String :




throwArgumentMismatch(String, v)

)

)


).toList()
),
tags: src.getOrUndefinedMapped(_reflection.tagsPart.oasName, (v) => 
(

            
            v as List
            

)

.map((v) => Tag.deserialize
(

            v

)


).toList()
),
status: src.getOrUndefinedMapped(_reflection.statusPart.oasName, (v) => 
(

            
                    NewPetStatusEnum.$safe(( v is String ? v as String :




throwArgumentMismatch(String, v)

))

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

  if (!src.getOrUndefined(_reflection.idPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is int
     || (int.tryParse(v.toString()) != null)
    
    
    
)
),
    unDefined: () => !_reflection.idPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.categoryInlineAllofPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            NewPetCategoryInlineAllof.canDeserialize(v)
            
),
    unDefined: () => !_reflection.categoryInlineAllofPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.categoryAllOfRefPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            Category.canDeserialize(v)
            
),
    unDefined: () => !_reflection.categoryAllOfRefPart.required,
)) {
    return false;
  }
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
if (!src.getOrUndefined(_reflection.photoUrlsPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is List && v.every((v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
))
),
    unDefined: () => !_reflection.photoUrlsPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.tagsPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is List && v.every((v) => v == null ? false :
(

    
            Tag.canDeserialize(v)
            
))
),
    unDefined: () => !_reflection.tagsPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.statusPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
     && NewPetStatusEnum.canDeserialize(v)
)
),
    unDefined: () => !_reflection.statusPart.required,
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
Map<String, dynamic> _$NewPetSerialize(NewPet src) {
  Map<String, dynamic> initialResult = () {
    
      return _$NewPetToMap(src);
    
  }();
  return initialResult;
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

