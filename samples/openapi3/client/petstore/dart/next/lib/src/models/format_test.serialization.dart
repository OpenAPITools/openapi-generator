// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'format_test.dart';


//class serialization

Map<String, dynamic> _$FormatTestToMap(FormatTest instance) {
  final _reflection = FormatTestReflection.instance;
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
    if (instance.$double.isDefined)
    _reflection.$double.oasName: (
            double
 v) {
      return v;
    }(instance.$double.valueRequired),
    if (instance.decimal.isDefined)
    _reflection.decimal.oasName: (
            double
 v) {
      return v;
    }(instance.decimal.valueRequired),
    if (instance.string.isDefined)
    _reflection.string.oasName: (
            String
 v) {
      return v;
    }(instance.string.valueRequired),
    
    _reflection.byte.oasName: (
            Uint8List
 v) {
      return v;
    }(instance.byte),
    if (instance.base64Str.isDefined)
    _reflection.base64Str.oasName: (
            String
 v) {
      return v;
    }(instance.base64Str.valueRequired),
    if (instance.binary.isDefined)
    _reflection.binary.oasName: (
            XFile
 v) {
      return v;
    }(instance.binary.valueRequired),
    
    _reflection.date.oasName: (
            DateTime
 v) {
      return v;
    }(instance.date),
    if (instance.dateTime.isDefined)
    _reflection.dateTime.oasName: (
            DateTime
 v) {
      return v;
    }(instance.dateTime.valueRequired),
    if (instance.uuid.isDefined)
    _reflection.uuid.oasName: (
            String
 v) {
      return v;
    }(instance.uuid.valueRequired),
    if (instance.uuidWithDefault.isDefined)
    _reflection.uuidWithDefault.oasName: (
            String
 v) {
      return v;
    }(instance.uuidWithDefault.valueRequired),
    
    _reflection.password.oasName: (
            String
 v) {
      return v;
    }(instance.password),
    if (instance.patternWithDigits.isDefined)
    _reflection.patternWithDigits.oasName: (
            String
 v) {
      return v;
    }(instance.patternWithDigits.valueRequired),
    if (instance.patternWithDigitsAndDelimiter.isDefined)
    _reflection.patternWithDigitsAndDelimiter.oasName: (
            String
 v) {
      return v;
    }(instance.patternWithDigitsAndDelimiter.valueRequired),
    
    
  };
}

FormatTest _$FormatTestFromMap(Map<String, dynamic> src) {
  final _reflection = FormatTestReflection.instance;
  return FormatTest.$all(
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
$double: src.getOrUndefinedMapped(_reflection.$double.oasName, (v) => 
(

    
            
                    v as double
            

)


),
decimal: src.getOrUndefinedMapped(_reflection.decimal.oasName, (v) => 
(

    
            
                    v as double
            

)


),
string: src.getOrUndefinedMapped(_reflection.string.oasName, (v) => 
(

    
            
                    v as String
            

)


),
byte: src.getRequiredMapped(_reflection.byte.oasName, (v) => 
(

    
            
                    v as Uint8List
            

)


),
base64Str: src.getOrUndefinedMapped(_reflection.base64Str.oasName, (v) => 
(

    
            
                    v as String
            

)


),
binary: src.getOrUndefinedMapped(_reflection.binary.oasName, (v) => 
(

    
            
                    v as XFile
            

)


),
date: src.getRequiredMapped(_reflection.date.oasName, (v) => 
(

    
            
                    v as DateTime
            

)


),
dateTime: src.getOrUndefinedMapped(_reflection.dateTime.oasName, (v) => 
(

    
            
                    v as DateTime
            

)


),
uuid: src.getOrUndefinedMapped(_reflection.uuid.oasName, (v) => 
(

    
            
                    v as String
            

)


),
uuidWithDefault: src.getOrUndefinedMapped(_reflection.uuidWithDefault.oasName, (v) => 
(

    
            
                    v as String
            

)


),
password: src.getRequiredMapped(_reflection.password.oasName, (v) => 
(

    
            
                    v as String
            

)


),
patternWithDigits: src.getOrUndefinedMapped(_reflection.patternWithDigits.oasName, (v) => 
(

    
            
                    v as String
            

)


),
patternWithDigitsAndDelimiter: src.getOrUndefinedMapped(_reflection.patternWithDigitsAndDelimiter.oasName, (v) => 
(

    
            
                    v as String
            

)


),
    
    
  );
}

bool _$FormatTestCanFromMap(Map<String, dynamic> src) {
  final _reflection = FormatTestReflection.instance;
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
if (!src.getOrUndefined(_reflection.decimal.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is double
),
    unDefined: () => !_reflection.decimal.required,
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
if (!src.getOrUndefined(_reflection.byte.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is Uint8List
),
    unDefined: () => !_reflection.byte.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.base64Str.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.base64Str.required,
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
if (!src.getOrUndefined(_reflection.uuid.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.uuid.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.uuidWithDefault.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.uuidWithDefault.required,
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
if (!src.getOrUndefined(_reflection.patternWithDigits.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.patternWithDigits.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.patternWithDigitsAndDelimiter.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.patternWithDigitsAndDelimiter.required,
)) {
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
Object? _$FormatTestSerialize(FormatTest src) {
  
  return src.toMap();
  
  
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

