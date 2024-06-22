// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'health_check_result.dart';


//class serialization

Map<String, dynamic> _$HealthCheckResultToMap(HealthCheckResult instance) {
  final _reflection = HealthCheckResultReflection.instance;
  return <String, dynamic>{
    if (instance.nullableMessage.isDefined)
    _reflection.nullableMessage.oasName: (
            String
? v) {
      return v;
    }(instance.nullableMessage.valueRequired),
    
    
  };
}

HealthCheckResult _$HealthCheckResultFromMap(Map<String, dynamic> src) {
  final _reflection = HealthCheckResultReflection.instance;
  return HealthCheckResult.$all(
    nullableMessage: src.getOrUndefinedMapped(_reflection.nullableMessage.oasName, (v) => 
(

    
            
                    v as String
            ?

)


),
    
    
  );
}

bool _$HealthCheckResultCanFromMap(Map<String, dynamic> src) {
  final _reflection = HealthCheckResultReflection.instance;
  if (!src.getOrUndefined(_reflection.nullableMessage.oasName).split<bool>(
    defined: (v) => v == null ? true :
(

    
            
            v is String
),
    unDefined: () => !_reflection.nullableMessage.required,
)) {
    return false;
  }
  
  return true;
}

/// Deserializes a primitive Object (num, String, List, Map).
HealthCheckResult _$HealthCheckResultDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$HealthCheckResultFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$HealthCheckResultCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$HealthCheckResultCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Object? _$HealthCheckResultSerialize(HealthCheckResult src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$HealthCheckResultToXml(HealthCheckResult instance) {
  final reflection = HealthCheckResultXmlReflection.instance;
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

HealthCheckResult _$HealthCheckResultFromXml(XmlElement src) {
  final reflection = HealthCheckResultXmlReflection.instance;
  return HealthCheckResult.$all(

  );
}
*/

