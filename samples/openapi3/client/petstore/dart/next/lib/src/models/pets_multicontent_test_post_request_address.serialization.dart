// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'pets_multicontent_test_post_request_address.dart';


//class serialization

Map<String, dynamic> _$PetsMulticontentTestPostRequestAddressToMap(PetsMulticontentTestPostRequestAddress instance) {
  final _reflection = PetsMulticontentTestPostRequestAddressReflection.instance;
  return <String, dynamic>{
    if (instance.street.isDefined)
    _reflection.street.oasName: (
            String
 v) {
      return v;
    }(instance.street.valueRequired),
    if (instance.city.isDefined)
    _reflection.city.oasName: (
            String
 v) {
      return v;
    }(instance.city.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

PetsMulticontentTestPostRequestAddress _$PetsMulticontentTestPostRequestAddressFromMap(Map<String, dynamic> src) {
  final _reflection = PetsMulticontentTestPostRequestAddressReflection.instance;
  return PetsMulticontentTestPostRequestAddress.$all(
    street: src.getOrUndefinedMapped(_reflection.street.oasName, (v) => 
(

    
            
                    v as String
            

)


),
city: src.getOrUndefinedMapped(_reflection.city.oasName, (v) => 
(

    
            
                    v as String
            

)


),
    additionalProperties: AdditionalProperties(src.except(_reflection.knownKeys).map((key, v) => MapEntry(key, 
(
v

)
))),
    
  );
}

bool _$PetsMulticontentTestPostRequestAddressCanFromMap(Map<String, dynamic> src) {
  final _reflection = PetsMulticontentTestPostRequestAddressReflection.instance;
  if (!src.getOrUndefined(_reflection.street.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.street.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.city.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.city.required,
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
PetsMulticontentTestPostRequestAddress _$PetsMulticontentTestPostRequestAddressDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$PetsMulticontentTestPostRequestAddressFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$PetsMulticontentTestPostRequestAddressCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$PetsMulticontentTestPostRequestAddressCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Map<String,dynamic> _$PetsMulticontentTestPostRequestAddressSerialize(PetsMulticontentTestPostRequestAddress src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$PetsMulticontentTestPostRequestAddressToXml(PetsMulticontentTestPostRequestAddress instance) {
  final reflection = PetsMulticontentTestPostRequestAddressXmlReflection.instance;
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

PetsMulticontentTestPostRequestAddress _$PetsMulticontentTestPostRequestAddressFromXml(XmlElement src) {
  final reflection = PetsMulticontentTestPostRequestAddressXmlReflection.instance;
  return PetsMulticontentTestPostRequestAddress.$all(

  );
}
*/

