// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'test_endpoint_parameters_request.dart';


//class serialization

Map<String, dynamic> _$TestEndpointParametersRequestToMap(TestEndpointParametersRequest instance) {
  final _reflection = TestEndpointParametersRequestReflection.instance;
  return <String, dynamic>{
    if (instance.integer.isDefined)
    _reflection.integerPart.oasName: (
            int

 v) {
      return v;
    }(instance.integer.valueRequired),
    if (instance.int32.isDefined)
    _reflection.int32Part.oasName: (
            int

 v) {
      return v;
    }(instance.int32.valueRequired),
    if (instance.int64.isDefined)
    _reflection.int64Part.oasName: (
            int

 v) {
      return v;
    }(instance.int64.valueRequired),
    
    _reflection.numberPart.oasName: (
            num

 v) {
      return v;
    }(instance.number),
    if (instance.float.isDefined)
    _reflection.floatPart.oasName: (
            double

 v) {
      return v;
    }(instance.float.valueRequired),
    
    _reflection.$doublePart.oasName: (
            double

 v) {
      return v;
    }(instance.$double),
    if (instance.string.isDefined)
    _reflection.stringPart.oasName: (
            String

 v) {
      return v;
    }(instance.string.valueRequired),
    
    _reflection.patternWithoutDelimiterPart.oasName: (
            String

 v) {
      return v;
    }(instance.patternWithoutDelimiter),
    
    _reflection.bytePart.oasName: (
            Uint8List

 v) {
      return v;
    }(instance.byte),
    if (instance.binary.isDefined)
    _reflection.binaryPart.oasName: (
            XFile

 v) {
      return v;
    }(instance.binary.valueRequired),
    if (instance.date.isDefined)
    _reflection.datePart.oasName: (
            DateTime

 v) {
      return v;
    }(instance.date.valueRequired),
    if (instance.dateTime.isDefined)
    _reflection.dateTimePart.oasName: (
            DateTime

 v) {
      return v;
    }(instance.dateTime.valueRequired),
    if (instance.password.isDefined)
    _reflection.passwordPart.oasName: (
            String

 v) {
      return v;
    }(instance.password.valueRequired),
    if (instance.callback.isDefined)
    _reflection.callbackPart.oasName: (
            String

 v) {
      return v;
    }(instance.callback.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

TestEndpointParametersRequest _$TestEndpointParametersRequestFromMap(Map<String, dynamic> src) {
  const _reflection = TestEndpointParametersRequestReflection.instance;
  return TestEndpointParametersRequest.$all(
    integer: src.getOrUndefinedMapped(_reflection.integerPart.oasName, (v) => 
(

            
                    ( v is int ? v as int :
int.parse(v.toString())



)

)


),
int32: src.getOrUndefinedMapped(_reflection.int32Part.oasName, (v) => 
(

            
                    ( v is int ? v as int :
int.parse(v.toString())



)

)


),
int64: src.getOrUndefinedMapped(_reflection.int64Part.oasName, (v) => 
(

            
                    ( v is int ? v as int :
int.parse(v.toString())



)

)


),
number: src.getRequiredMapped(_reflection.numberPart.oasName, (v) => 
(

            
                    ( v is num ? v as num :
num.parse(v.toString())



)

)


),
float: src.getOrUndefinedMapped(_reflection.floatPart.oasName, (v) => 
(

            
                    ( v is double ? v as double :
double.parse(v.toString())



)

)


),
$double: src.getRequiredMapped(_reflection.$doublePart.oasName, (v) => 
(

            
                    ( v is double ? v as double :
double.parse(v.toString())



)

)


),
string: src.getOrUndefinedMapped(_reflection.stringPart.oasName, (v) => 
(

            
                    ( v is String ? v as String :




throwArgumentMismatch(String, v)

)

)


),
patternWithoutDelimiter: src.getRequiredMapped(_reflection.patternWithoutDelimiterPart.oasName, (v) => 
(

            
                    ( v is String ? v as String :




throwArgumentMismatch(String, v)

)

)


),
byte: src.getRequiredMapped(_reflection.bytePart.oasName, (v) => 
(

            
                    ( v is Uint8List ? v as Uint8List :




throwArgumentMismatch(Uint8List, v)

)

)


),
binary: src.getOrUndefinedMapped(_reflection.binaryPart.oasName, (v) => 
(

            
                    ( v is XFile ? v as XFile :




throwArgumentMismatch(XFile, v)

)

)


),
date: src.getOrUndefinedMapped(_reflection.datePart.oasName, (v) => 
(

            
                    ( v is DateTime ? v as DateTime :




throwArgumentMismatch(DateTime, v)

)

)


),
dateTime: src.getOrUndefinedMapped(_reflection.dateTimePart.oasName, (v) => 
(

            
                    ( v is DateTime ? v as DateTime :


v is int ? DateTime.fromMillisecondsSinceEpoch(v as int) : DateTime.parse(v.toString())

)

)


),
password: src.getOrUndefinedMapped(_reflection.passwordPart.oasName, (v) => 
(

            
                    ( v is String ? v as String :




throwArgumentMismatch(String, v)

)

)


),
callback: src.getOrUndefinedMapped(_reflection.callbackPart.oasName, (v) => 
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

bool _$TestEndpointParametersRequestCanFromMap(Map<String, dynamic> src) {
  final _reflection = TestEndpointParametersRequestReflection.instance;

  if (!src.getOrUndefined(_reflection.integerPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is int
     || (int.tryParse(v.toString()) != null)
    
    
    
)
),
    unDefined: () => !_reflection.integerPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.int32Part.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is int
     || (int.tryParse(v.toString()) != null)
    
    
    
)
),
    unDefined: () => !_reflection.int32Part.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.int64Part.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is int
     || (int.tryParse(v.toString()) != null)
    
    
    
)
),
    unDefined: () => !_reflection.int64Part.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.numberPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is num
     || (num.tryParse(v.toString()) != null)
    
    
    
)
),
    unDefined: () => !_reflection.numberPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.floatPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is double
     || (double.tryParse(v.toString()) != null)
    
    
    
)
),
    unDefined: () => !_reflection.floatPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.$doublePart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is double
     || (double.tryParse(v.toString()) != null)
    
    
    
)
),
    unDefined: () => !_reflection.$doublePart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.stringPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
),
    unDefined: () => !_reflection.stringPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.patternWithoutDelimiterPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
),
    unDefined: () => !_reflection.patternWithoutDelimiterPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.bytePart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is Uint8List
    
    
    
    
)
),
    unDefined: () => !_reflection.bytePart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.binaryPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is XFile
    
    
    
    
)
),
    unDefined: () => !_reflection.binaryPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.datePart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is DateTime
    
    
    
    
)
),
    unDefined: () => !_reflection.datePart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.dateTimePart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is DateTime
    
    
     || (v is int || DateTime.tryParse(v.toString()) != null)
    
)
),
    unDefined: () => !_reflection.dateTimePart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.passwordPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
),
    unDefined: () => !_reflection.passwordPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.callbackPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
),
    unDefined: () => !_reflection.callbackPart.required,
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
Map<String, dynamic> _$TestEndpointParametersRequestSerialize(TestEndpointParametersRequest src) {
  Map<String, dynamic> initialResult = () {
    
      return _$TestEndpointParametersRequestToMap(src);
    
  }();
  return initialResult;
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

