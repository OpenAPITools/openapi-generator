// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'update_pet_with_form_request.dart';


//class serialization

Map<String, dynamic> _$UpdatePetWithFormRequestToMap(UpdatePetWithFormRequest instance) {
  final _reflection = UpdatePetWithFormRequestReflection.instance;
  return <String, dynamic>{
    if (instance.name.isDefined)
    _reflection.name.oasName: (
            String
 v) {
      return v;
    }(instance.name.valueRequired),
    if (instance.status.isDefined)
    _reflection.status.oasName: (
            String
 v) {
      return v;
    }(instance.status.valueRequired),
    
    
  };
}

UpdatePetWithFormRequest _$UpdatePetWithFormRequestFromMap(Map<String, dynamic> src) {
  final _reflection = UpdatePetWithFormRequestReflection.instance;
  return UpdatePetWithFormRequest.$all(
    name: src.getOrUndefinedMapped(_reflection.name.oasName, (v) => 
(

    
            
                    v as String
            

)


),
status: src.getOrUndefinedMapped(_reflection.status.oasName, (v) => 
(

    
            
                    v as String
            

)


),
    
    
  );
}

bool _$UpdatePetWithFormRequestCanFromMap(Map<String, dynamic> src) {
  final _reflection = UpdatePetWithFormRequestReflection.instance;
  if (!src.getOrUndefined(_reflection.name.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.name.required,
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
UpdatePetWithFormRequest _$UpdatePetWithFormRequestDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$UpdatePetWithFormRequestFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$UpdatePetWithFormRequestCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$UpdatePetWithFormRequestCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Object? _$UpdatePetWithFormRequestSerialize(UpdatePetWithFormRequest src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$UpdatePetWithFormRequestToXml(UpdatePetWithFormRequest instance) {
  final reflection = UpdatePetWithFormRequestXmlReflection.instance;
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

UpdatePetWithFormRequest _$UpdatePetWithFormRequestFromXml(XmlElement src) {
  final reflection = UpdatePetWithFormRequestXmlReflection.instance;
  return UpdatePetWithFormRequest.$all(

  );
}
*/

