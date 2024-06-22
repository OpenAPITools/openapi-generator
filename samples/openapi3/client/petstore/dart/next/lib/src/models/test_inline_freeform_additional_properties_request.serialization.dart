// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'test_inline_freeform_additional_properties_request.dart';


//class serialization

Map<String, dynamic> _$TestInlineFreeformAdditionalPropertiesRequestToMap(TestInlineFreeformAdditionalPropertiesRequest instance) {
  final _reflection = TestInlineFreeformAdditionalPropertiesRequestReflection.instance;
  return <String, dynamic>{
    if (instance.someProperty.isDefined)
    _reflection.someProperty.oasName: (
            String
 v) {
      return v;
    }(instance.someProperty.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

TestInlineFreeformAdditionalPropertiesRequest _$TestInlineFreeformAdditionalPropertiesRequestFromMap(Map<String, dynamic> src) {
  final _reflection = TestInlineFreeformAdditionalPropertiesRequestReflection.instance;
  return TestInlineFreeformAdditionalPropertiesRequest.$all(
    someProperty: src.getOrUndefinedMapped(_reflection.someProperty.oasName, (v) => 
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

bool _$TestInlineFreeformAdditionalPropertiesRequestCanFromMap(Map<String, dynamic> src) {
  final _reflection = TestInlineFreeformAdditionalPropertiesRequestReflection.instance;
  if (!src.getOrUndefined(_reflection.someProperty.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.someProperty.required,
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
TestInlineFreeformAdditionalPropertiesRequest _$TestInlineFreeformAdditionalPropertiesRequestDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$TestInlineFreeformAdditionalPropertiesRequestFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$TestInlineFreeformAdditionalPropertiesRequestCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$TestInlineFreeformAdditionalPropertiesRequestCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Object? _$TestInlineFreeformAdditionalPropertiesRequestSerialize(TestInlineFreeformAdditionalPropertiesRequest src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$TestInlineFreeformAdditionalPropertiesRequestToXml(TestInlineFreeformAdditionalPropertiesRequest instance) {
  final reflection = TestInlineFreeformAdditionalPropertiesRequestXmlReflection.instance;
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

TestInlineFreeformAdditionalPropertiesRequest _$TestInlineFreeformAdditionalPropertiesRequestFromXml(XmlElement src) {
  final reflection = TestInlineFreeformAdditionalPropertiesRequestXmlReflection.instance;
  return TestInlineFreeformAdditionalPropertiesRequest.$all(

  );
}
*/

