// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'has_only_read_only.dart';


//class serialization

Map<String, dynamic> _$HasOnlyReadOnlyToMap(HasOnlyReadOnly instance) {
  final _reflection = HasOnlyReadOnlyReflection.instance;
  return <String, dynamic>{
    if (instance.bar.isDefined)
    _reflection.barPart.oasName: (
            String

 v) {
      return v;
    }(instance.bar.valueRequired),
    if (instance.foo.isDefined)
    _reflection.fooPart.oasName: (
            String

 v) {
      return v;
    }(instance.foo.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

HasOnlyReadOnly _$HasOnlyReadOnlyFromMap(Map<String, dynamic> src) {
  const _reflection = HasOnlyReadOnlyReflection.instance;
  return HasOnlyReadOnly.$all(
    bar: src.getOrUndefinedMapped(_reflection.barPart.oasName, (v) => 
(

            
                    ( v is String ? v as String :




throwArgumentMismatch(String, v)

)

)


),
foo: src.getOrUndefinedMapped(_reflection.fooPart.oasName, (v) => 
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

bool _$HasOnlyReadOnlyCanFromMap(Map<String, dynamic> src) {
  final _reflection = HasOnlyReadOnlyReflection.instance;

  if (!src.getOrUndefined(_reflection.barPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
),
    unDefined: () => !_reflection.barPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.fooPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
),
    unDefined: () => !_reflection.fooPart.required,
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
HasOnlyReadOnly _$HasOnlyReadOnlyDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$HasOnlyReadOnlyFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$HasOnlyReadOnlyCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$HasOnlyReadOnlyCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Map<String, dynamic> _$HasOnlyReadOnlySerialize(HasOnlyReadOnly src) {
  Map<String, dynamic> initialResult = () {
    
      return _$HasOnlyReadOnlyToMap(src);
    
  }();
  return initialResult;
}


/*
XmlElement _$HasOnlyReadOnlyToXml(HasOnlyReadOnly instance) {
  final reflection = HasOnlyReadOnlyXmlReflection.instance;
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

HasOnlyReadOnly _$HasOnlyReadOnlyFromXml(XmlElement src) {
  final reflection = HasOnlyReadOnlyXmlReflection.instance;
  return HasOnlyReadOnly.$all(

  );
}
*/

