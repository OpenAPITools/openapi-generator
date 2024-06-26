// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'read_only_first.dart';


//class serialization

Map<String, dynamic> _$ReadOnlyFirstToMap(ReadOnlyFirst instance) {
  final _reflection = ReadOnlyFirstReflection.instance;
  return <String, dynamic>{
    if (instance.bar.isDefined)
    _reflection.bar.oasName: (
            String
 v) {
      return v;
    }(instance.bar.valueRequired),
    if (instance.baz.isDefined)
    _reflection.baz.oasName: (
            String
 v) {
      return v;
    }(instance.baz.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

ReadOnlyFirst _$ReadOnlyFirstFromMap(Map<String, dynamic> src) {
  final _reflection = ReadOnlyFirstReflection.instance;
  return ReadOnlyFirst.$all(
    bar: src.getOrUndefinedMapped(_reflection.bar.oasName, (v) => 
(

    
            
                    v as String
            

)


),
baz: src.getOrUndefinedMapped(_reflection.baz.oasName, (v) => 
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

bool _$ReadOnlyFirstCanFromMap(Map<String, dynamic> src) {
  final _reflection = ReadOnlyFirstReflection.instance;
  if (!src.getOrUndefined(_reflection.bar.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.bar.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.baz.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.baz.required,
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
ReadOnlyFirst _$ReadOnlyFirstDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$ReadOnlyFirstFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$ReadOnlyFirstCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$ReadOnlyFirstCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Object? _$ReadOnlyFirstSerialize(ReadOnlyFirst src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$ReadOnlyFirstToXml(ReadOnlyFirst instance) {
  final reflection = ReadOnlyFirstXmlReflection.instance;
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

ReadOnlyFirst _$ReadOnlyFirstFromXml(XmlElement src) {
  final reflection = ReadOnlyFirstXmlReflection.instance;
  return ReadOnlyFirst.$all(

  );
}
*/

