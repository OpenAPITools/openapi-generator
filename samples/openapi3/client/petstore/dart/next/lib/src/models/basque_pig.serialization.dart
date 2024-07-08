// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'basque_pig.dart';


//class serialization

Map<String, dynamic> _$BasquePigToMap(BasquePig instance) {
  final _reflection = BasquePigReflection.instance;
  return <String, dynamic>{
    
    _reflection.classNamePart.oasName: (
            String

 v) {
      return v;
    }(instance.className),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

BasquePig _$BasquePigFromMap(Map<String, dynamic> src) {
  const _reflection = BasquePigReflection.instance;
  return BasquePig.$all(
    className: src.getRequiredMapped(_reflection.classNamePart.oasName, (v) => 
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

bool _$BasquePigCanFromMap(Map<String, dynamic> src) {
  final _reflection = BasquePigReflection.instance;

  if (!src.getOrUndefined(_reflection.classNamePart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
),
    unDefined: () => !_reflection.classNamePart.required,
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
BasquePig _$BasquePigDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$BasquePigFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$BasquePigCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$BasquePigCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Map<String, dynamic> _$BasquePigSerialize(BasquePig src) {
  Map<String, dynamic> initialResult = () {
    
      return _$BasquePigToMap(src);
    
  }();
  return initialResult;
}


/*
XmlElement _$BasquePigToXml(BasquePig instance) {
  final reflection = BasquePigXmlReflection.instance;
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

BasquePig _$BasquePigFromXml(XmlElement src) {
  final reflection = BasquePigXmlReflection.instance;
  return BasquePig.$all(

  );
}
*/

