// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'new_pet_category_inline_allof_all_of_category_tag.dart';


//class serialization

Map<String, dynamic> _$NewPetCategoryInlineAllofAllOfCategoryTagToMap(NewPetCategoryInlineAllofAllOfCategoryTag instance) {
  final _reflection = NewPetCategoryInlineAllofAllOfCategoryTagReflection.instance;
  return <String, dynamic>{
    if (instance.id.isDefined)
    _reflection.idPart.oasName: (
            int

 v) {
      return v;
    }(instance.id.valueRequired),
    if (instance.name.isDefined)
    _reflection.namePart.oasName: (
            String

 v) {
      return v;
    }(instance.name.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

NewPetCategoryInlineAllofAllOfCategoryTag _$NewPetCategoryInlineAllofAllOfCategoryTagFromMap(Map<String, dynamic> src) {
  const _reflection = NewPetCategoryInlineAllofAllOfCategoryTagReflection.instance;
  return NewPetCategoryInlineAllofAllOfCategoryTag.$all(
    id: src.getOrUndefinedMapped(_reflection.idPart.oasName, (v) => 
(

            
                    ( v is int ? v as int :
int.parse(v.toString())



)

)


),
name: src.getOrUndefinedMapped(_reflection.namePart.oasName, (v) => 
(

            
                    ( v is String ? v as String :




throwArgumentMismatch(String, v)

)

)


),
    additionalProperties: AdditionalProperties(src.except(_reflection.knownKeys).map((key, v) => MapEntry(key, 
(
v

)
))),
    
  );
}

bool _$NewPetCategoryInlineAllofAllOfCategoryTagCanFromMap(Map<String, dynamic> src) {
  final _reflection = NewPetCategoryInlineAllofAllOfCategoryTagReflection.instance;

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
  if (!src.except(_reflection.knownKeys).values.every((v) => v == null ? true :
(
true
))) {
    return false;
  }

  return true;
}

/// Deserializes a primitive Object (num, String, List, Map).
NewPetCategoryInlineAllofAllOfCategoryTag _$NewPetCategoryInlineAllofAllOfCategoryTagDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$NewPetCategoryInlineAllofAllOfCategoryTagFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$NewPetCategoryInlineAllofAllOfCategoryTagCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$NewPetCategoryInlineAllofAllOfCategoryTagCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Map<String, dynamic> _$NewPetCategoryInlineAllofAllOfCategoryTagSerialize(NewPetCategoryInlineAllofAllOfCategoryTag src) {
  Map<String, dynamic> initialResult = () {
    
      return _$NewPetCategoryInlineAllofAllOfCategoryTagToMap(src);
    
  }();
  return initialResult;
}


/*
XmlElement _$NewPetCategoryInlineAllofAllOfCategoryTagToXml(NewPetCategoryInlineAllofAllOfCategoryTag instance) {
  final reflection = NewPetCategoryInlineAllofAllOfCategoryTagXmlReflection.instance;
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

NewPetCategoryInlineAllofAllOfCategoryTag _$NewPetCategoryInlineAllofAllOfCategoryTagFromXml(XmlElement src) {
  final reflection = NewPetCategoryInlineAllofAllOfCategoryTagXmlReflection.instance;
  return NewPetCategoryInlineAllofAllOfCategoryTag.$all(

  );
}
*/

