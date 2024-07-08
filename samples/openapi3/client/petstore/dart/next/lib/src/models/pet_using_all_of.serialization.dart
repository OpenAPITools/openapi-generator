// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'pet_using_all_of.dart';


//class serialization

Map<String, dynamic> _$PetUsingAllOfToMap(PetUsingAllOf instance) {
  final _reflection = PetUsingAllOfReflection.instance;
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
            PetUsingAllOfStatusEnum

 v) {
      return v.value;
    }(instance.status.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

PetUsingAllOf _$PetUsingAllOfFromMap(Map<String, dynamic> src) {
  const _reflection = PetUsingAllOfReflection.instance;
  return PetUsingAllOf.$all(
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

            
                    PetUsingAllOfStatusEnum.$safe(( v is String ? v as String :




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

bool _$PetUsingAllOfCanFromMap(Map<String, dynamic> src) {
  final _reflection = PetUsingAllOfReflection.instance;

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
    
    
    
     && PetUsingAllOfStatusEnum.canDeserialize(v)
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
Map<String, dynamic> _$PetUsingAllOfSerialize(PetUsingAllOf src) {
  Map<String, dynamic> initialResult = () {
    
      return _$PetUsingAllOfToMap(src);
    
  }();
  return initialResult;
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

