// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'capitalization.dart';


//class serialization

Map<String, dynamic> _$CapitalizationToMap(Capitalization instance) {
  final _reflection = CapitalizationReflection.instance;
  return <String, dynamic>{
    if (instance.smallCamel.isDefined)
    _reflection.smallCamel.oasName: (
            String
 v) {
      return v;
    }(instance.smallCamel.valueRequired),
    if (instance.capitalCamel.isDefined)
    _reflection.capitalCamel.oasName: (
            String
 v) {
      return v;
    }(instance.capitalCamel.valueRequired),
    if (instance.smallSnake.isDefined)
    _reflection.smallSnake.oasName: (
            String
 v) {
      return v;
    }(instance.smallSnake.valueRequired),
    if (instance.capitalSnake.isDefined)
    _reflection.capitalSnake.oasName: (
            String
 v) {
      return v;
    }(instance.capitalSnake.valueRequired),
    if (instance.scAETHFlowPoints.isDefined)
    _reflection.scAETHFlowPoints.oasName: (
            String
 v) {
      return v;
    }(instance.scAETHFlowPoints.valueRequired),
    if (instance.ATT_NAME.isDefined)
    _reflection.ATT_NAME.oasName: (
            String
 v) {
      return v;
    }(instance.ATT_NAME.valueRequired),
    
    
  };
}

Capitalization _$CapitalizationFromMap(Map<String, dynamic> src) {
  final _reflection = CapitalizationReflection.instance;
  return Capitalization.$all(
    smallCamel: src.getOrUndefinedMapped(_reflection.smallCamel.oasName, (v) => 
(

    
            
                    v as String
            

)


),
capitalCamel: src.getOrUndefinedMapped(_reflection.capitalCamel.oasName, (v) => 
(

    
            
                    v as String
            

)


),
smallSnake: src.getOrUndefinedMapped(_reflection.smallSnake.oasName, (v) => 
(

    
            
                    v as String
            

)


),
capitalSnake: src.getOrUndefinedMapped(_reflection.capitalSnake.oasName, (v) => 
(

    
            
                    v as String
            

)


),
scAETHFlowPoints: src.getOrUndefinedMapped(_reflection.scAETHFlowPoints.oasName, (v) => 
(

    
            
                    v as String
            

)


),
ATT_NAME: src.getOrUndefinedMapped(_reflection.ATT_NAME.oasName, (v) => 
(

    
            
                    v as String
            

)


),
    
    
  );
}

bool _$CapitalizationCanFromMap(Map<String, dynamic> src) {
  final _reflection = CapitalizationReflection.instance;
  if (!src.getOrUndefined(_reflection.smallCamel.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.smallCamel.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.capitalCamel.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.capitalCamel.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.smallSnake.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.smallSnake.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.capitalSnake.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.capitalSnake.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.scAETHFlowPoints.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.scAETHFlowPoints.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.ATT_NAME.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.ATT_NAME.required,
)) {
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
Object? _$CapitalizationSerialize(Capitalization src) {
  
  return src.toMap();
  
  
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

