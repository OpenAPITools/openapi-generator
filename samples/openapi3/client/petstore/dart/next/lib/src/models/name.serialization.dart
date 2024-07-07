// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'name.dart';


//class serialization

Map<String, dynamic> _$NameToMap(Name instance) {
  final _reflection = NameReflection.instance;
  return <String, dynamic>{
    
    _reflection.namePart.oasName: (
            int
 v) {
      return v;
    }(instance.name),
    if (instance.snakeCase.isDefined)
    _reflection.snakeCasePart.oasName: (
            int
 v) {
      return v;
    }(instance.snakeCase.valueRequired),
    if (instance.property.isDefined)
    _reflection.propertyPart.oasName: (
            String
 v) {
      return v;
    }(instance.property.valueRequired),
    if (instance.$123number.isDefined)
    _reflection.$123numberPart.oasName: (
            int
 v) {
      return v;
    }(instance.$123number.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

Name _$NameFromMap(Map<String, dynamic> src) {
  const _reflection = NameReflection.instance;
  return Name.$all(
    name: src.getRequiredMapped(_reflection.namePart.oasName, (v) => 
(

            
                    ( v is int ? v as int :
int.parse(v.toString())



)

)


),
snakeCase: src.getOrUndefinedMapped(_reflection.snakeCasePart.oasName, (v) => 
(

            
                    ( v is int ? v as int :
int.parse(v.toString())



)

)


),
property: src.getOrUndefinedMapped(_reflection.propertyPart.oasName, (v) => 
(

            
                    ( v is String ? v as String :




throwArgumentMismatch(String, v)

)

)


),
$123number: src.getOrUndefinedMapped(_reflection.$123numberPart.oasName, (v) => 
(

            
                    ( v is int ? v as int :
int.parse(v.toString())



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

bool _$NameCanFromMap(Map<String, dynamic> src) {
  final _reflection = NameReflection.instance;

  if (!src.getOrUndefined(_reflection.namePart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is int
     || (int.tryParse(v.toString()) != null)
    
    
    
)
),
    unDefined: () => !_reflection.namePart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.snakeCasePart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is int
     || (int.tryParse(v.toString()) != null)
    
    
    
)
),
    unDefined: () => !_reflection.snakeCasePart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.propertyPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
),
    unDefined: () => !_reflection.propertyPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.$123numberPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is int
     || (int.tryParse(v.toString()) != null)
    
    
    
)
),
    unDefined: () => !_reflection.$123numberPart.required,
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
Name _$NameDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$NameFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$NameCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$NameCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Map<String, dynamic> _$NameSerialize(Name src) {
  Map<String, dynamic> initialResult = () {
    
    return src.toMap();
    
    
  }();
  return initialResult;
}


/*
XmlElement _$NameToXml(Name instance) {
  final reflection = NameXmlReflection.instance;
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

Name _$NameFromXml(XmlElement src) {
  final reflection = NameXmlReflection.instance;
  return Name.$all(

  );
}
*/

