// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'format_test.dart';


//class serialization

Map<String, dynamic> _$FormatTestToMap(FormatTest instance) {
  final _reflection = FormatTestReflection.instance;
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
    if (instance.$double.isDefined)
    _reflection.$doublePart.oasName: (
            double

 v) {
      return v;
    }(instance.$double.valueRequired),
    if (instance.decimal.isDefined)
    _reflection.decimalPart.oasName: (
            double

 v) {
      return v;
    }(instance.decimal.valueRequired),
    if (instance.string.isDefined)
    _reflection.stringPart.oasName: (
            String

 v) {
      return v;
    }(instance.string.valueRequired),
    
    _reflection.bytePart.oasName: (
            Uint8List

 v) {
      return v;
    }(instance.byte),
    if (instance.base64Str.isDefined)
    _reflection.base64StrPart.oasName: (
            String

 v) {
      return v;
    }(instance.base64Str.valueRequired),
    if (instance.binary.isDefined)
    _reflection.binaryPart.oasName: (
            XFile

 v) {
      return v;
    }(instance.binary.valueRequired),
    
    _reflection.datePart.oasName: (
            DateTime

 v) {
      return v;
    }(instance.date),
    if (instance.dateTime.isDefined)
    _reflection.dateTimePart.oasName: (
            DateTime

 v) {
      return v;
    }(instance.dateTime.valueRequired),
    if (instance.uuid.isDefined)
    _reflection.uuidPart.oasName: (
            String

 v) {
      return v;
    }(instance.uuid.valueRequired),
    if (instance.uuidWithDefault.isDefined)
    _reflection.uuidWithDefaultPart.oasName: (
            String

 v) {
      return v;
    }(instance.uuidWithDefault.valueRequired),
    
    _reflection.passwordPart.oasName: (
            String

 v) {
      return v;
    }(instance.password),
    if (instance.patternWithDigits.isDefined)
    _reflection.patternWithDigitsPart.oasName: (
            String

 v) {
      return v;
    }(instance.patternWithDigits.valueRequired),
    if (instance.patternWithDigitsAndDelimiter.isDefined)
    _reflection.patternWithDigitsAndDelimiterPart.oasName: (
            String

 v) {
      return v;
    }(instance.patternWithDigitsAndDelimiter.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

FormatTest _$FormatTestFromMap(Map<String, dynamic> src) {
  const _reflection = FormatTestReflection.instance;
  return FormatTest.$all(
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
$double: src.getOrUndefinedMapped(_reflection.$doublePart.oasName, (v) => 
(

            
                    ( v is double ? v as double :
double.parse(v.toString())



)

)


),
decimal: src.getOrUndefinedMapped(_reflection.decimalPart.oasName, (v) => 
(

            
                    ( v is double ? v as double :




throwArgumentMismatch(double, v)

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
byte: src.getRequiredMapped(_reflection.bytePart.oasName, (v) => 
(

            
                    ( v is Uint8List ? v as Uint8List :




throwArgumentMismatch(Uint8List, v)

)

)


),
base64Str: src.getOrUndefinedMapped(_reflection.base64StrPart.oasName, (v) => 
(

            
                    ( v is String ? v as String :




throwArgumentMismatch(String, v)

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
date: src.getRequiredMapped(_reflection.datePart.oasName, (v) => 
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
uuid: src.getOrUndefinedMapped(_reflection.uuidPart.oasName, (v) => 
(

            
                    ( v is String ? v as String :




throwArgumentMismatch(String, v)

)

)


),
uuidWithDefault: src.getOrUndefinedMapped(_reflection.uuidWithDefaultPart.oasName, (v) => 
(

            
                    ( v is String ? v as String :




throwArgumentMismatch(String, v)

)

)


),
password: src.getRequiredMapped(_reflection.passwordPart.oasName, (v) => 
(

            
                    ( v is String ? v as String :




throwArgumentMismatch(String, v)

)

)


),
patternWithDigits: src.getOrUndefinedMapped(_reflection.patternWithDigitsPart.oasName, (v) => 
(

            
                    ( v is String ? v as String :




throwArgumentMismatch(String, v)

)

)


),
patternWithDigitsAndDelimiter: src.getOrUndefinedMapped(_reflection.patternWithDigitsAndDelimiterPart.oasName, (v) => 
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

bool _$FormatTestCanFromMap(Map<String, dynamic> src) {
  final _reflection = FormatTestReflection.instance;

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
if (!src.getOrUndefined(_reflection.decimalPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is double
    
    
    
    
)
),
    unDefined: () => !_reflection.decimalPart.required,
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
if (!src.getOrUndefined(_reflection.base64StrPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
),
    unDefined: () => !_reflection.base64StrPart.required,
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
if (!src.getOrUndefined(_reflection.uuidPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
),
    unDefined: () => !_reflection.uuidPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.uuidWithDefaultPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
),
    unDefined: () => !_reflection.uuidWithDefaultPart.required,
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
if (!src.getOrUndefined(_reflection.patternWithDigitsPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
),
    unDefined: () => !_reflection.patternWithDigitsPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.patternWithDigitsAndDelimiterPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
),
    unDefined: () => !_reflection.patternWithDigitsAndDelimiterPart.required,
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
FormatTest _$FormatTestDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$FormatTestFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$FormatTestCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$FormatTestCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Map<String, dynamic> _$FormatTestSerialize(FormatTest src) {
  Map<String, dynamic> initialResult = () {
    
      return _$FormatTestToMap(src);
    
  }();
  return initialResult;
}


/*
XmlElement _$FormatTestToXml(FormatTest instance) {
  final reflection = FormatTestXmlReflection.instance;
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

FormatTest _$FormatTestFromXml(XmlElement src) {
  final reflection = FormatTestXmlReflection.instance;
  return FormatTest.$all(

  );
}
*/

