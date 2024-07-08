// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'api_response.dart';


//class serialization

Map<String, dynamic> _$ApiResponseToMap(ApiResponse instance) {
  final _reflection = ApiResponseReflection.instance;
  return <String, dynamic>{
    if (instance.code.isDefined)
    _reflection.codePart.oasName: (
            int

 v) {
      return v;
    }(instance.code.valueRequired),
    if (instance.type.isDefined)
    _reflection.typePart.oasName: (
            String

 v) {
      return v;
    }(instance.type.valueRequired),
    if (instance.message.isDefined)
    _reflection.messagePart.oasName: (
            String

 v) {
      return v;
    }(instance.message.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

ApiResponse _$ApiResponseFromMap(Map<String, dynamic> src) {
  const _reflection = ApiResponseReflection.instance;
  return ApiResponse.$all(
    code: src.getOrUndefinedMapped(_reflection.codePart.oasName, (v) => 
(

            
                    ( v is int ? v as int :
int.parse(v.toString())



)

)


),
type: src.getOrUndefinedMapped(_reflection.typePart.oasName, (v) => 
(

            
                    ( v is String ? v as String :




throwArgumentMismatch(String, v)

)

)


),
message: src.getOrUndefinedMapped(_reflection.messagePart.oasName, (v) => 
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

bool _$ApiResponseCanFromMap(Map<String, dynamic> src) {
  final _reflection = ApiResponseReflection.instance;

  if (!src.getOrUndefined(_reflection.codePart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is int
     || (int.tryParse(v.toString()) != null)
    
    
    
)
),
    unDefined: () => !_reflection.codePart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.typePart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
),
    unDefined: () => !_reflection.typePart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.messagePart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
),
    unDefined: () => !_reflection.messagePart.required,
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
Map<String, dynamic> _$ApiResponseSerialize(ApiResponse src) {
  Map<String, dynamic> initialResult = () {
    
      return _$ApiResponseToMap(src);
    
  }();
  return initialResult;
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

