// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'number_only.dart';


//class serialization

Map<String, dynamic> _$NumberOnlyToMap(NumberOnly instance) {
  final _reflection = NumberOnlyReflection.instance;
  return <String, dynamic>{
    if (instance.justNumber.isDefined)
    _reflection.justNumberPart.oasName: (
            num
 v) {
      return v;
    }(instance.justNumber.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

NumberOnly _$NumberOnlyFromMap(Map<String, dynamic> src) {
  const _reflection = NumberOnlyReflection.instance;
  return NumberOnly.$all(
    justNumber: src.getOrUndefinedMapped(_reflection.justNumberPart.oasName, (v) => 
(

            
                    ( v is num ? v as num :
num.parse(v.toString())



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

bool _$NumberOnlyCanFromMap(Map<String, dynamic> src) {
  final _reflection = NumberOnlyReflection.instance;

  if (!src.getOrUndefined(_reflection.justNumberPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is num
     || (num.tryParse(v.toString()) != null)
    
    
    
)
),
    unDefined: () => !_reflection.justNumberPart.required,
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
NumberOnly _$NumberOnlyDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$NumberOnlyFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$NumberOnlyCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$NumberOnlyCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Map<String, dynamic> _$NumberOnlySerialize(NumberOnly src) {
  Map<String, dynamic> initialResult = () {
    
    return src.toMap();
    
    
  }();
  return initialResult;
}


/*
XmlElement _$NumberOnlyToXml(NumberOnly instance) {
  final reflection = NumberOnlyXmlReflection.instance;
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

NumberOnly _$NumberOnlyFromXml(XmlElement src) {
  final reflection = NumberOnlyXmlReflection.instance;
  return NumberOnly.$all(

  );
}
*/

