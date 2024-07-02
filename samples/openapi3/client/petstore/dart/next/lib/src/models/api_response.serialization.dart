// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'api_response.dart';


//class serialization

Map<String, dynamic> _$ApiResponseToMap(ApiResponse instance) {
  final _reflection = ApiResponseReflection.instance;
  return <String, dynamic>{
    if (instance.code.isDefined)
    _reflection.code.oasName: (
            int
 v) {
      return v;
    }(instance.code.valueRequired),
    if (instance.type.isDefined)
    _reflection.type.oasName: (
            String
 v) {
      return v;
    }(instance.type.valueRequired),
    if (instance.message.isDefined)
    _reflection.message.oasName: (
            String
 v) {
      return v;
    }(instance.message.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

ApiResponse _$ApiResponseFromMap(Map<String, dynamic> src) {
  final _reflection = ApiResponseReflection.instance;
  return ApiResponse.$all(
    code: src.getOrUndefinedMapped(_reflection.code.oasName, (v) => 
(

    
            
                    v as int
            

)


),
type: src.getOrUndefinedMapped(_reflection.type.oasName, (v) => 
(

    
            
                    v as String
            

)


),
message: src.getOrUndefinedMapped(_reflection.message.oasName, (v) => 
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

bool _$ApiResponseCanFromMap(Map<String, dynamic> src) {
  final _reflection = ApiResponseReflection.instance;
  if (!src.getOrUndefined(_reflection.code.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is int
),
    unDefined: () => !_reflection.code.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.type.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.type.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.message.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.message.required,
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
ApiResponse _$ApiResponseDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$ApiResponseFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$ApiResponseCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$ApiResponseCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Map<String,dynamic> _$ApiResponseSerialize(ApiResponse src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$ApiResponseToXml(ApiResponse instance) {
  final reflection = ApiResponseXmlReflection.instance;
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

ApiResponse _$ApiResponseFromXml(XmlElement src) {
  final reflection = ApiResponseXmlReflection.instance;
  return ApiResponse.$all(

  );
}
*/

