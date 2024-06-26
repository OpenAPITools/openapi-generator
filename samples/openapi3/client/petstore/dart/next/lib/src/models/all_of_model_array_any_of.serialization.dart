// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'all_of_model_array_any_of.dart';


//class serialization

Map<String, dynamic> _$AllOfModelArrayAnyOfToMap(AllOfModelArrayAnyOf instance) {
  final _reflection = AllOfModelArrayAnyOfReflection.instance;
  return <String, dynamic>{
    
    _reflection.name.oasName: (
            String
 v) {
      return v;
    }(instance.name),
    if (instance.attributes.isDefined)
    _reflection.attributes.oasName: (
            AllOfModelArrayAnyOfAllOfAttributes
 v) {
      return v.serialize();
    }(instance.attributes.valueRequired),
    if (instance.id.isDefined)
    _reflection.id.oasName: (
            int
 v) {
      return v;
    }(instance.id.valueRequired),
    if (instance.linkListColumn1.isDefined)
    _reflection.linkListColumn1.oasName: (
            AllOfModelArrayAnyOfAllOfLinkListColumn1
 v) {
      return v.serialize();
    }(instance.linkListColumn1.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

AllOfModelArrayAnyOf _$AllOfModelArrayAnyOfFromMap(Map<String, dynamic> src) {
  final _reflection = AllOfModelArrayAnyOfReflection.instance;
  return AllOfModelArrayAnyOf.$all(
    name: src.getRequiredMapped(_reflection.name.oasName, (v) => 
(

    
            
                    v as String
            

)


),
attributes: src.getOrUndefinedMapped(_reflection.attributes.oasName, (v) => AllOfModelArrayAnyOfAllOfAttributes.deserialize
(

    
            v


)


),
id: src.getOrUndefinedMapped(_reflection.id.oasName, (v) => 
(

    
            
                    v as int
            

)


),
linkListColumn1: src.getOrUndefinedMapped(_reflection.linkListColumn1.oasName, (v) => AllOfModelArrayAnyOfAllOfLinkListColumn1.deserialize
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

bool _$AllOfModelArrayAnyOfCanFromMap(Map<String, dynamic> src) {
  final _reflection = AllOfModelArrayAnyOfReflection.instance;
  if (!src.getOrUndefined(_reflection.name.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.name.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.attributes.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            AllOfModelArrayAnyOfAllOfAttributes.canDeserialize(v)
            
),
    unDefined: () => !_reflection.attributes.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.id.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is int
),
    unDefined: () => !_reflection.id.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.linkListColumn1.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            AllOfModelArrayAnyOfAllOfLinkListColumn1.canDeserialize(v)
            
),
    unDefined: () => !_reflection.linkListColumn1.required,
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
AllOfModelArrayAnyOf _$AllOfModelArrayAnyOfDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$AllOfModelArrayAnyOfFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$AllOfModelArrayAnyOfCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$AllOfModelArrayAnyOfCanFromMap(src);
  } else {
    final v = src;
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Object? _$AllOfModelArrayAnyOfSerialize(AllOfModelArrayAnyOf src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$AllOfModelArrayAnyOfToXml(AllOfModelArrayAnyOf instance) {
  final reflection = AllOfModelArrayAnyOfXmlReflection.instance;
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

AllOfModelArrayAnyOf _$AllOfModelArrayAnyOfFromXml(XmlElement src) {
  final reflection = AllOfModelArrayAnyOfXmlReflection.instance;
  return AllOfModelArrayAnyOf.$all(

  );
}
*/

