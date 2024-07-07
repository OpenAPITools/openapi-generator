// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'new_pet_category_inline_allof.dart';


//class serialization

Map<String, dynamic> _$NewPetCategoryInlineAllofToMap(NewPetCategoryInlineAllof instance) {
  final _reflection = NewPetCategoryInlineAllofReflection.instance;
  return <String, dynamic>{
    if (instance.id.isDefined)
    _reflection.idPart.oasName: (
            int
 v) {
      return v;
    }(instance.id.valueRequired),
    
    _reflection.namePart.oasName: (
            String
 v) {
      return v;
    }(instance.name),
    if (instance.categoryTag.isDefined)
    _reflection.categoryTagPart.oasName: (
            NewPetCategoryInlineAllofAllOfCategoryTag
 v) {
      return v.serialize();
    }(instance.categoryTag.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

NewPetCategoryInlineAllof _$NewPetCategoryInlineAllofFromMap(Map<String, dynamic> src) {
  const _reflection = NewPetCategoryInlineAllofReflection.instance;
  return NewPetCategoryInlineAllof.$all(
    id: src.getOrUndefinedMapped(_reflection.idPart.oasName, (v) => 
(

            
                    ( v is int ? v as int :
int.parse(v.toString())



)

)


),
name: src.getRequiredMapped(_reflection.namePart.oasName, (v) => 
(

            
                    ( v is String ? v as String :




throwArgumentMismatch(String, v)

)

)


),
categoryTag: src.getOrUndefinedMapped(_reflection.categoryTagPart.oasName, (v) => NewPetCategoryInlineAllofAllOfCategoryTag.deserialize
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
if (!src.getOrUndefined(_reflection.categoryTagPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            NewPetCategoryInlineAllofAllOfCategoryTag.canDeserialize(v)
            
),
    unDefined: () => !_reflection.categoryTagPart.required,
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
Map<String, dynamic> _$NewPetCategoryInlineAllofSerialize(NewPetCategoryInlineAllof src) {
  Map<String, dynamic> initialResult = () {
    
    return src.toMap();
    
    
  }();
  return initialResult;
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

