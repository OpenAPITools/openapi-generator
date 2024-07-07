// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'update_pet_with_form_request.dart';


//class serialization

Map<String, dynamic> _$UpdatePetWithFormRequestToMap(UpdatePetWithFormRequest instance) {
  final _reflection = UpdatePetWithFormRequestReflection.instance;
  return <String, dynamic>{
    if (instance.name.isDefined)
    _reflection.namePart.oasName: (
            String
 v) {
      return v;
    }(instance.name.valueRequired),
    if (instance.status.isDefined)
    _reflection.statusPart.oasName: (
            String
 v) {
      return v;
    }(instance.status.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

UpdatePetWithFormRequest _$UpdatePetWithFormRequestFromMap(Map<String, dynamic> src) {
  const _reflection = UpdatePetWithFormRequestReflection.instance;
  return UpdatePetWithFormRequest.$all(
    name: src.getOrUndefinedMapped(_reflection.namePart.oasName, (v) => 
(

            
                    ( v is String ? v as String :




throwArgumentMismatch(String, v)

)

)


),
status: src.getOrUndefinedMapped(_reflection.statusPart.oasName, (v) => 
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

bool _$UpdatePetWithFormRequestCanFromMap(Map<String, dynamic> src) {
  final _reflection = UpdatePetWithFormRequestReflection.instance;

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
if (!src.getOrUndefined(_reflection.statusPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
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
Map<String, dynamic> _$UpdatePetWithFormRequestSerialize(UpdatePetWithFormRequest src) {
  Map<String, dynamic> initialResult = () {
    
    return src.toMap();
    
    
  }();
  return initialResult;
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

