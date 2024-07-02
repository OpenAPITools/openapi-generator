// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'test_json_form_data_request.dart';


//class serialization

Map<String, dynamic> _$TestJsonFormDataRequestToMap(TestJsonFormDataRequest instance) {
  final _reflection = TestJsonFormDataRequestReflection.instance;
  return <String, dynamic>{
    
    _reflection.param.oasName: (
            String
 v) {
      return v;
    }(instance.param),
    
    _reflection.param2.oasName: (
            String
 v) {
      return v;
    }(instance.param2),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

TestJsonFormDataRequest _$TestJsonFormDataRequestFromMap(Map<String, dynamic> src) {
  final _reflection = TestJsonFormDataRequestReflection.instance;
  return TestJsonFormDataRequest.$all(
    param: src.getRequiredMapped(_reflection.param.oasName, (v) => 
(

    
            
                    v as String
            

)


),
param2: src.getRequiredMapped(_reflection.param2.oasName, (v) => 
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

bool _$TestJsonFormDataRequestCanFromMap(Map<String, dynamic> src) {
  final _reflection = TestJsonFormDataRequestReflection.instance;
  if (!src.getOrUndefined(_reflection.param.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.param.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.param2.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.param2.required,
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
TestJsonFormDataRequest _$TestJsonFormDataRequestDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$TestJsonFormDataRequestFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$TestJsonFormDataRequestCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$TestJsonFormDataRequestCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Map<String,dynamic> _$TestJsonFormDataRequestSerialize(TestJsonFormDataRequest src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$TestJsonFormDataRequestToXml(TestJsonFormDataRequest instance) {
  final reflection = TestJsonFormDataRequestXmlReflection.instance;
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

TestJsonFormDataRequest _$TestJsonFormDataRequestFromXml(XmlElement src) {
  final reflection = TestJsonFormDataRequestXmlReflection.instance;
  return TestJsonFormDataRequest.$all(

  );
}
*/

