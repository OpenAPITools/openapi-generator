// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'capitalization.dart';


//class serialization

Map<String, dynamic> _$CapitalizationToMap(Capitalization instance) {
  final _reflection = CapitalizationReflection.instance;
  return <String, dynamic>{
    if (instance.smallCamel.isDefined)
    _reflection.smallCamelPart.oasName: (
            String

 v) {
      return v;
    }(instance.smallCamel.valueRequired),
    if (instance.capitalCamel.isDefined)
    _reflection.capitalCamelPart.oasName: (
            String

 v) {
      return v;
    }(instance.capitalCamel.valueRequired),
    if (instance.smallSnake.isDefined)
    _reflection.smallSnakePart.oasName: (
            String

 v) {
      return v;
    }(instance.smallSnake.valueRequired),
    if (instance.capitalSnake.isDefined)
    _reflection.capitalSnakePart.oasName: (
            String

 v) {
      return v;
    }(instance.capitalSnake.valueRequired),
    if (instance.scAETHFlowPoints.isDefined)
    _reflection.scAETHFlowPointsPart.oasName: (
            String

 v) {
      return v;
    }(instance.scAETHFlowPoints.valueRequired),
    if (instance.ATT_NAME.isDefined)
    _reflection.ATT_NAMEPart.oasName: (
            String

 v) {
      return v;
    }(instance.ATT_NAME.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

Capitalization _$CapitalizationFromMap(Map<String, dynamic> src) {
  const _reflection = CapitalizationReflection.instance;
  return Capitalization.$all(
    smallCamel: src.getOrUndefinedMapped(_reflection.smallCamelPart.oasName, (v) => 
(

            
                    ( v is String ? v as String :




throwArgumentMismatch(String, v)

)

)


),
capitalCamel: src.getOrUndefinedMapped(_reflection.capitalCamelPart.oasName, (v) => 
(

            
                    ( v is String ? v as String :




throwArgumentMismatch(String, v)

)

)


),
smallSnake: src.getOrUndefinedMapped(_reflection.smallSnakePart.oasName, (v) => 
(

            
                    ( v is String ? v as String :




throwArgumentMismatch(String, v)

)

)


),
capitalSnake: src.getOrUndefinedMapped(_reflection.capitalSnakePart.oasName, (v) => 
(

            
                    ( v is String ? v as String :




throwArgumentMismatch(String, v)

)

)


),
scAETHFlowPoints: src.getOrUndefinedMapped(_reflection.scAETHFlowPointsPart.oasName, (v) => 
(

            
                    ( v is String ? v as String :




throwArgumentMismatch(String, v)

)

)


),
ATT_NAME: src.getOrUndefinedMapped(_reflection.ATT_NAMEPart.oasName, (v) => 
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

bool _$CapitalizationCanFromMap(Map<String, dynamic> src) {
  final _reflection = CapitalizationReflection.instance;

  if (!src.getOrUndefined(_reflection.smallCamelPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
),
    unDefined: () => !_reflection.smallCamelPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.capitalCamelPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
),
    unDefined: () => !_reflection.capitalCamelPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.smallSnakePart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
),
    unDefined: () => !_reflection.smallSnakePart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.capitalSnakePart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
),
    unDefined: () => !_reflection.capitalSnakePart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.scAETHFlowPointsPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
),
    unDefined: () => !_reflection.scAETHFlowPointsPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.ATT_NAMEPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
),
    unDefined: () => !_reflection.ATT_NAMEPart.required,
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
Capitalization _$CapitalizationDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$CapitalizationFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$CapitalizationCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$CapitalizationCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Map<String, dynamic> _$CapitalizationSerialize(Capitalization src) {
  Map<String, dynamic> initialResult = () {
    
      return _$CapitalizationToMap(src);
    
  }();
  return initialResult;
}


/*
XmlElement _$CapitalizationToXml(Capitalization instance) {
  final reflection = CapitalizationXmlReflection.instance;
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

Capitalization _$CapitalizationFromXml(XmlElement src) {
  final reflection = CapitalizationXmlReflection.instance;
  return Capitalization.$all(

  );
}
*/

