// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'deprecated_object.dart';


//class serialization

Map<String, dynamic> _$DeprecatedObjectToMap(DeprecatedObject instance) {
  final _reflection = DeprecatedObjectReflection.instance;
  return <String, dynamic>{
    if (instance.name.isDefined)
    _reflection.namePart.oasName: (
            String
 v) {
      return v;
    }(instance.name.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

DeprecatedObject _$DeprecatedObjectFromMap(Map<String, dynamic> src) {
  const _reflection = DeprecatedObjectReflection.instance;
  return DeprecatedObject.$all(
    name: src.getOrUndefinedMapped(_reflection.namePart.oasName, (v) => 
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

bool _$DeprecatedObjectCanFromMap(Map<String, dynamic> src) {
  final _reflection = DeprecatedObjectReflection.instance;

  if (!src.getOrUndefined(_reflection.namePart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
),
    unDefined: () => !_reflection.namePart.required,
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
DeprecatedObject _$DeprecatedObjectDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$DeprecatedObjectFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$DeprecatedObjectCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$DeprecatedObjectCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Map<String, dynamic> _$DeprecatedObjectSerialize(DeprecatedObject src) {
  Map<String, dynamic> initialResult = () {
    
    return src.toMap();
    
    
  }();
  return initialResult;
}


/*
XmlElement _$DeprecatedObjectToXml(DeprecatedObject instance) {
  final reflection = DeprecatedObjectXmlReflection.instance;
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

DeprecatedObject _$DeprecatedObjectFromXml(XmlElement src) {
  final reflection = DeprecatedObjectXmlReflection.instance;
  return DeprecatedObject.$all(

  );
}
*/

