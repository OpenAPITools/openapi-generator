// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'test_enum_parameters_request.dart';


//class serialization

Map<String, dynamic> _$TestEnumParametersRequestToMap(TestEnumParametersRequest instance) {
  final _reflection = TestEnumParametersRequestReflection.instance;
  return <String, dynamic>{
    if (instance.enumFormStringArray.isDefined)
    _reflection.enumFormStringArray.oasName: (
    List<
        
            TestEnumParametersRequestEnumFormStringArrayEnum
>
 v) {
      return v.map((v) => v.value).toList();
    }(instance.enumFormStringArray.valueRequired),
    if (instance.enumFormString.isDefined)
    _reflection.enumFormString.oasName: (
            TestEnumParametersRequestEnumFormStringEnum
 v) {
      return v.value;
    }(instance.enumFormString.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

TestEnumParametersRequest _$TestEnumParametersRequestFromMap(Map<String, dynamic> src) {
  final _reflection = TestEnumParametersRequestReflection.instance;
  return TestEnumParametersRequest.$all(
    enumFormStringArray: src.getOrUndefinedMapped(_reflection.enumFormStringArray.oasName, (v) => 
(

    
            
            v as List
            

)

.map((v) => 
(

    
            
                    TestEnumParametersRequestEnumFormStringArrayEnum.$safe(v as String)
            

)


).toList()
),
enumFormString: src.getOrUndefinedMapped(_reflection.enumFormString.oasName, (v) => 
(

    
            
                    TestEnumParametersRequestEnumFormStringEnum.$safe(v as String)
            

)


),
    additionalProperties: AdditionalProperties(src.except(_reflection.knownKeys).map((key, v) => MapEntry(key, 
(
v

)
))),
    
  );
}

bool _$TestEnumParametersRequestCanFromMap(Map<String, dynamic> src) {
  final _reflection = TestEnumParametersRequestReflection.instance;
  if (!src.getOrUndefined(_reflection.enumFormStringArray.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is List && v.every((v) => v == null ? false :
(

    
            
            v is String
))
),
    unDefined: () => !_reflection.enumFormStringArray.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.enumFormString.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.enumFormString.required,
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
TestEnumParametersRequest _$TestEnumParametersRequestDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$TestEnumParametersRequestFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$TestEnumParametersRequestCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$TestEnumParametersRequestCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Map<String,dynamic> _$TestEnumParametersRequestSerialize(TestEnumParametersRequest src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$TestEnumParametersRequestToXml(TestEnumParametersRequest instance) {
  final reflection = TestEnumParametersRequestXmlReflection.instance;
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

TestEnumParametersRequest _$TestEnumParametersRequestFromXml(XmlElement src) {
  final reflection = TestEnumParametersRequestXmlReflection.instance;
  return TestEnumParametersRequest.$all(

  );
}
*/

