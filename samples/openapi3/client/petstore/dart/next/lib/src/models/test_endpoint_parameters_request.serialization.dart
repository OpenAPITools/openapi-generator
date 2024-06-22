// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'test_endpoint_parameters_request.dart';


//class serialization

Map<String, dynamic> _$TestEndpointParametersRequestToMap(TestEndpointParametersRequest instance) {
  final _reflection = TestEndpointParametersRequestReflection.instance;
  return <String, dynamic>{
    if (instance.integer.isDefined)
    _reflection.integer.oasName: (
            int
 v) {
      return v;
    }(instance.integer.valueRequired),
    if (instance.int32.isDefined)
    _reflection.int32.oasName: (
            int
 v) {
      return v;
    }(instance.int32.valueRequired),
    if (instance.int64.isDefined)
    _reflection.int64.oasName: (
            int
 v) {
      return v;
    }(instance.int64.valueRequired),
    
    _reflection.number.oasName: (
            num
 v) {
      return v;
    }(instance.number),
    if (instance.float.isDefined)
    _reflection.float.oasName: (
            double
 v) {
      return v;
    }(instance.float.valueRequired),
    
    _reflection.$double.oasName: (
            double
 v) {
      return v;
    }(instance.$double),
    if (instance.string.isDefined)
    _reflection.string.oasName: (
            String
 v) {
      return v;
    }(instance.string.valueRequired),
    
    _reflection.patternWithoutDelimiter.oasName: (
            String
 v) {
      return v;
    }(instance.patternWithoutDelimiter),
    
    _reflection.byte.oasName: (
            Uint8List
 v) {
      return v;
    }(instance.byte),
    if (instance.binary.isDefined)
    _reflection.binary.oasName: (
            XFile
 v) {
      return v;
    }(instance.binary.valueRequired),
    if (instance.date.isDefined)
    _reflection.date.oasName: (
            DateTime
 v) {
      return v;
    }(instance.date.valueRequired),
    if (instance.dateTime.isDefined)
    _reflection.dateTime.oasName: (
            DateTime
 v) {
      return v;
    }(instance.dateTime.valueRequired),
    if (instance.password.isDefined)
    _reflection.password.oasName: (
            String
 v) {
      return v;
    }(instance.password.valueRequired),
    if (instance.callback.isDefined)
    _reflection.callback.oasName: (
            String
 v) {
      return v;
    }(instance.callback.valueRequired),
    
    
  };
}

TestEndpointParametersRequest _$TestEndpointParametersRequestFromMap(Map<String, dynamic> src) {
  final _reflection = TestEndpointParametersRequestReflection.instance;
  return TestEndpointParametersRequest.$all(
    integer: src.getOrUndefinedMapped(_reflection.integer.oasName, (v) => 
(

    
            
                    v as int
            

)


),
int32: src.getOrUndefinedMapped(_reflection.int32.oasName, (v) => 
(

    
            
                    v as int
            

)


),
int64: src.getOrUndefinedMapped(_reflection.int64.oasName, (v) => 
(

    
            
                    v as int
            

)


),
number: src.getRequiredMapped(_reflection.number.oasName, (v) => 
(

    
            
                    v as num
            

)


),
float: src.getOrUndefinedMapped(_reflection.float.oasName, (v) => 
(

    
            
                    v as double
            

)


),
$double: src.getRequiredMapped(_reflection.$double.oasName, (v) => 
(

    
            
                    v as double
            

)


),
string: src.getOrUndefinedMapped(_reflection.string.oasName, (v) => 
(

    
            
                    v as String
            

)


),
patternWithoutDelimiter: src.getRequiredMapped(_reflection.patternWithoutDelimiter.oasName, (v) => 
(

    
            
                    v as String
            

)


),
byte: src.getRequiredMapped(_reflection.byte.oasName, (v) => 
(

    
            
                    v as Uint8List
            

)


),
binary: src.getOrUndefinedMapped(_reflection.binary.oasName, (v) => 
(

    
            
                    v as XFile
            

)


),
date: src.getOrUndefinedMapped(_reflection.date.oasName, (v) => 
(

    
            
                    v as DateTime
            

)


),
dateTime: src.getOrUndefinedMapped(_reflection.dateTime.oasName, (v) => 
(

    
            
                    v as DateTime
            

)


),
password: src.getOrUndefinedMapped(_reflection.password.oasName, (v) => 
(

    
            
                    v as String
            

)


),
callback: src.getOrUndefinedMapped(_reflection.callback.oasName, (v) => 
(

    
            
                    v as String
            

)


),
    
    
  );
}

bool _$TestEndpointParametersRequestCanFromMap(Map<String, dynamic> src) {
  final _reflection = TestEndpointParametersRequestReflection.instance;
  if (!src.getOrUndefined(_reflection.integer.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is int
),
    unDefined: () => !_reflection.integer.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.int32.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is int
),
    unDefined: () => !_reflection.int32.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.int64.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is int
),
    unDefined: () => !_reflection.int64.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.number.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is num
),
    unDefined: () => !_reflection.number.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.float.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is double
),
    unDefined: () => !_reflection.float.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.$double.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is double
),
    unDefined: () => !_reflection.$double.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.string.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.string.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.patternWithoutDelimiter.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.patternWithoutDelimiter.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.byte.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is Uint8List
),
    unDefined: () => !_reflection.byte.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.binary.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is XFile
),
    unDefined: () => !_reflection.binary.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.date.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is DateTime
),
    unDefined: () => !_reflection.date.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.dateTime.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is DateTime
),
    unDefined: () => !_reflection.dateTime.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.password.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.password.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.callback.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.callback.required,
)) {
    return false;
  }
  
  return true;
}

/// Deserializes a primitive Object (num, String, List, Map).
TestEndpointParametersRequest _$TestEndpointParametersRequestDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$TestEndpointParametersRequestFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$TestEndpointParametersRequestCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$TestEndpointParametersRequestCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Object? _$TestEndpointParametersRequestSerialize(TestEndpointParametersRequest src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$TestEndpointParametersRequestToXml(TestEndpointParametersRequest instance) {
  final reflection = TestEndpointParametersRequestXmlReflection.instance;
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

TestEndpointParametersRequest _$TestEndpointParametersRequestFromXml(XmlElement src) {
  final reflection = TestEndpointParametersRequestXmlReflection.instance;
  return TestEndpointParametersRequest.$all(

  );
}
*/

