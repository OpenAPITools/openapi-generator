// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'pet.dart';


//class serialization

Map<String, dynamic> _$PetToMap(Pet instance) {
  final _reflection = PetReflection.instance;
  return <String, dynamic>{
    if (instance.id.isDefined)
    _reflection.idPart.oasName: (
            int
 v) {
      return v;
    }(instance.id.valueRequired),
    if (instance.category.isDefined)
    _reflection.categoryPart.oasName: (
            Category
 v) {
      return v.serialize();
    }(instance.category.valueRequired),
    
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
            PetStatusEnum
 v) {
      return v.value;
    }(instance.status.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

Pet _$PetFromMap(Map<String, dynamic> src) {
  const _reflection = PetReflection.instance;
  return Pet.$all(
    id: src.getOrUndefinedMapped(_reflection.idPart.oasName, (v) => 
(

            
                    ( v is int ? v as int :
int.parse(v.toString())



)

)


),
category: src.getOrUndefinedMapped(_reflection.categoryPart.oasName, (v) => Category.deserialize
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

            
                    PetStatusEnum.$safe(( v is String ? v as String :




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

bool _$PetCanFromMap(Map<String, dynamic> src) {
  final _reflection = PetReflection.instance;

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
if (!src.getOrUndefined(_reflection.categoryPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            Category.canDeserialize(v)
            
),
    unDefined: () => !_reflection.categoryPart.required,
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
    
    
    
     && PetStatusEnum.canDeserialize(v)
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
Map<String, dynamic> _$PetSerialize(Pet src) {
  Map<String, dynamic> initialResult = () {
    
    return src.toMap();
    
    
  }();
  return initialResult;
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

