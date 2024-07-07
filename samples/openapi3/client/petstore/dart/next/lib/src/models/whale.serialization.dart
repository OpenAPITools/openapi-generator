// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'whale.dart';


//class serialization

Map<String, dynamic> _$WhaleToMap(Whale instance) {
  final _reflection = WhaleReflection.instance;
  return <String, dynamic>{
    if (instance.hasBaleen.isDefined)
    _reflection.hasBaleenPart.oasName: (
            bool
 v) {
      return v;
    }(instance.hasBaleen.valueRequired),
    if (instance.hasTeeth.isDefined)
    _reflection.hasTeethPart.oasName: (
            bool
 v) {
      return v;
    }(instance.hasTeeth.valueRequired),
    
    _reflection.classNamePart.oasName: (
            String
 v) {
      return v;
    }(instance.className),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

Whale _$WhaleFromMap(Map<String, dynamic> src) {
  const _reflection = WhaleReflection.instance;
  return Whale.$all(
    hasBaleen: src.getOrUndefinedMapped(_reflection.hasBaleenPart.oasName, (v) => 
(

            
                    ( v is bool ? v as bool :

bool.parse(v.toString())


)

)


),
hasTeeth: src.getOrUndefinedMapped(_reflection.hasTeethPart.oasName, (v) => 
(

            
                    ( v is bool ? v as bool :

bool.parse(v.toString())


)

)


),
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

bool _$WhaleCanFromMap(Map<String, dynamic> src) {
  final _reflection = WhaleReflection.instance;

  if (!src.getOrUndefined(_reflection.hasBaleenPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is bool
    
     || (bool.tryParse(v.toString()) != null)
    
    
)
),
    unDefined: () => !_reflection.hasBaleenPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.hasTeethPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is bool
    
     || (bool.tryParse(v.toString()) != null)
    
    
)
),
    unDefined: () => !_reflection.hasTeethPart.required,
)) {
    return false;
  }
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
Whale _$WhaleDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$WhaleFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$WhaleCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$WhaleCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Map<String, dynamic> _$WhaleSerialize(Whale src) {
  Map<String, dynamic> initialResult = () {
    
    return src.toMap();
    
    
  }();
  return initialResult;
}


/*
XmlElement _$WhaleToXml(Whale instance) {
  final reflection = WhaleXmlReflection.instance;
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

Whale _$WhaleFromXml(XmlElement src) {
  final reflection = WhaleXmlReflection.instance;
  return Whale.$all(

  );
}
*/

