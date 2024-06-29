// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'new_pet_category_inline_allof.dart';


//class serialization

Map<String, dynamic> _$NewPetCategoryInlineAllofToMap(NewPetCategoryInlineAllof instance) {
  final _reflection = NewPetCategoryInlineAllofReflection.instance;
  return <String, dynamic>{
    if (instance.id.isDefined)
    _reflection.id.oasName: (
            int
 v) {
      return v;
    }(instance.id.valueRequired),
    
    _reflection.name.oasName: (
            String
 v) {
      return v;
    }(instance.name),
    if (instance.categoryTag.isDefined)
    _reflection.categoryTag.oasName: (
            NewPetCategoryInlineAllofAllOfCategoryTag
 v) {
      return v.serialize();
    }(instance.categoryTag.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

NewPetCategoryInlineAllof _$NewPetCategoryInlineAllofFromMap(Map<String, dynamic> src) {
  final _reflection = NewPetCategoryInlineAllofReflection.instance;
  return NewPetCategoryInlineAllof.$all(
    id: src.getOrUndefinedMapped(_reflection.id.oasName, (v) => 
(

    
            
                    v as int
            

)


),
name: src.getRequiredMapped(_reflection.name.oasName, (v) => 
(

    
            
                    v as String
            

)


),
categoryTag: src.getOrUndefinedMapped(_reflection.categoryTag.oasName, (v) => NewPetCategoryInlineAllofAllOfCategoryTag.deserialize
(

    
            v


)


),
    additionalProperties: AdditionalProperties(src.except(_reflection.knownKeys).map((key, v) => MapEntry(key, 
(
v

)
))),
    
  );
}

bool _$NewPetCategoryInlineAllofCanFromMap(Map<String, dynamic> src) {
  final _reflection = NewPetCategoryInlineAllofReflection.instance;
  if (!src.getOrUndefined(_reflection.id.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is int
),
    unDefined: () => !_reflection.id.required,
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
if (!src.getOrUndefined(_reflection.categoryTag.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            NewPetCategoryInlineAllofAllOfCategoryTag.canDeserialize(v)
            
),
    unDefined: () => !_reflection.categoryTag.required,
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
NewPetCategoryInlineAllof _$NewPetCategoryInlineAllofDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$NewPetCategoryInlineAllofFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$NewPetCategoryInlineAllofCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$NewPetCategoryInlineAllofCanFromMap(src);
  } else {
    final v = src;
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Map<String,dynamic> _$NewPetCategoryInlineAllofSerialize(NewPetCategoryInlineAllof src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$NewPetCategoryInlineAllofToXml(NewPetCategoryInlineAllof instance) {
  final reflection = NewPetCategoryInlineAllofXmlReflection.instance;
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

NewPetCategoryInlineAllof _$NewPetCategoryInlineAllofFromXml(XmlElement src) {
  final reflection = NewPetCategoryInlineAllofXmlReflection.instance;
  return NewPetCategoryInlineAllof.$all(

  );
}
*/

