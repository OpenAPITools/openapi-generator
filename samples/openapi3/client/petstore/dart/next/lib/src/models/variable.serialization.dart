// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'variable.dart';


//class serialization

Map<String, dynamic> _$VariableToMap(Variable instance) {
  final _reflection = VariableReflection.instance;
  return <String, dynamic>{
    
    _reflection.namePart.oasName: (
            String

 v) {
      return v;
    }(instance.name),
    
    _reflection.valuePart.oasName: (
            Value

 v) {
      return v.serialize();
    }(instance.value),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

Variable _$VariableFromMap(Map<String, dynamic> src) {
  const _reflection = VariableReflection.instance;
  return Variable.$all(
    name: src.getRequiredMapped(_reflection.namePart.oasName, (v) => 
(

            
                    ( v is String ? v as String :




throwArgumentMismatch(String, v)

)

)


),
value: src.getRequiredMapped(_reflection.valuePart.oasName, (v) => Value.deserialize
(

            v

)


),
    additionalProperties: AdditionalProperties(src.except(_reflection.knownKeys).map((key, v) => MapEntry(key, 
(
v

)
))),
    
  );
}

bool _$VariableCanFromMap(Map<String, dynamic> src) {
  final _reflection = VariableReflection.instance;

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
if (!src.getOrUndefined(_reflection.valuePart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            Value.canDeserialize(v)
            
),
    unDefined: () => !_reflection.valuePart.required,
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
Variable _$VariableDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$VariableFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$VariableCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$VariableCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Map<String, dynamic> _$VariableSerialize(Variable src) {
  Map<String, dynamic> initialResult = () {
    
      return _$VariableToMap(src);
    
  }();
  return initialResult;
}


/*
XmlElement _$VariableToXml(Variable instance) {
  final reflection = VariableXmlReflection.instance;
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

Variable _$VariableFromXml(XmlElement src) {
  final reflection = VariableXmlReflection.instance;
  return Variable.$all(

  );
}
*/

