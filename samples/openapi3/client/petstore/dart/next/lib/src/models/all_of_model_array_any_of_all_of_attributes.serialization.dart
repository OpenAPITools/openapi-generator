// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'all_of_model_array_any_of_all_of_attributes.dart';


//class serialization

Map<String, dynamic> _$AllOfModelArrayAnyOfAllOfAttributesToMap(AllOfModelArrayAnyOfAllOfAttributes instance) {
  final _reflection = AllOfModelArrayAnyOfAllOfAttributesReflection.instance;
  return <String, dynamic>{
    if (instance.C.isDefined)
    _reflection.C.oasName: (
            AllOfModelArrayAnyOfAllOfAttributesC
 v) {
      return v.serialize();
    }(instance.C.valueRequired),
    
    
  };
}

AllOfModelArrayAnyOfAllOfAttributes _$AllOfModelArrayAnyOfAllOfAttributesFromMap(Map<String, dynamic> src) {
  final _reflection = AllOfModelArrayAnyOfAllOfAttributesReflection.instance;
  return AllOfModelArrayAnyOfAllOfAttributes.$all(
    C: src.getOrUndefinedMapped(_reflection.C.oasName, (v) => AllOfModelArrayAnyOfAllOfAttributesC.deserialize
(

    
            v


)


),
    
    
  );
}

bool _$AllOfModelArrayAnyOfAllOfAttributesCanFromMap(Map<String, dynamic> src) {
  final _reflection = AllOfModelArrayAnyOfAllOfAttributesReflection.instance;
  if (!src.getOrUndefined(_reflection.C.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            AllOfModelArrayAnyOfAllOfAttributesC.canDeserialize(v)
            
),
    unDefined: () => !_reflection.C.required,
)) {
    return false;
  }
  
  return true;
}

/// Deserializes a primitive Object (num, String, List, Map).
AllOfModelArrayAnyOfAllOfAttributes _$AllOfModelArrayAnyOfAllOfAttributesDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$AllOfModelArrayAnyOfAllOfAttributesFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$AllOfModelArrayAnyOfAllOfAttributesCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$AllOfModelArrayAnyOfAllOfAttributesCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Object? _$AllOfModelArrayAnyOfAllOfAttributesSerialize(AllOfModelArrayAnyOfAllOfAttributes src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$AllOfModelArrayAnyOfAllOfAttributesToXml(AllOfModelArrayAnyOfAllOfAttributes instance) {
  final reflection = AllOfModelArrayAnyOfAllOfAttributesXmlReflection.instance;
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

AllOfModelArrayAnyOfAllOfAttributes _$AllOfModelArrayAnyOfAllOfAttributesFromXml(XmlElement src) {
  final reflection = AllOfModelArrayAnyOfAllOfAttributesXmlReflection.instance;
  return AllOfModelArrayAnyOfAllOfAttributes.$all(

  );
}
*/

