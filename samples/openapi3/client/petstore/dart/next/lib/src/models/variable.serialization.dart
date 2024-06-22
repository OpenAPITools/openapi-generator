// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'variable.dart';


//class serialization

Map<String, dynamic> _$VariableToMap(Variable instance) {
  final _reflection = VariableReflection.instance;
  return <String, dynamic>{
    
    _reflection.name.oasName: (
            String
 v) {
      return v;
    }(instance.name),
    
    _reflection.value.oasName: (
            Value
 v) {
      return v.serialize();
    }(instance.value),
    
    
  };
}

Variable _$VariableFromMap(Map<String, dynamic> src) {
  final _reflection = VariableReflection.instance;
  return Variable.$all(
    name: src.getRequiredMapped(_reflection.name.oasName, (v) => 
(

    
            
                    v as String
            

)


),
value: src.getRequiredMapped(_reflection.value.oasName, (v) => Value.deserialize
(

    
            v


)


),
    
    
  );
}

bool _$VariableCanFromMap(Map<String, dynamic> src) {
  final _reflection = VariableReflection.instance;
  if (!src.getOrUndefined(_reflection.name.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.name.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.value.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            Value.canDeserialize(v)
            
),
    unDefined: () => !_reflection.value.required,
)) {
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
Object? _$VariableSerialize(Variable src) {
  
  return src.toMap();
  
  
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

