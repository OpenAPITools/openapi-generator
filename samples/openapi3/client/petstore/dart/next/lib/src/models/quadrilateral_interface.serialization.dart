// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'quadrilateral_interface.dart';


//class serialization

Map<String, dynamic> _$QuadrilateralInterfaceToMap(QuadrilateralInterface instance) {
  final _reflection = QuadrilateralInterfaceReflection.instance;
  return <String, dynamic>{
    
    _reflection.quadrilateralType.oasName: (
            String
 v) {
      return v;
    }(instance.quadrilateralType),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

QuadrilateralInterface _$QuadrilateralInterfaceFromMap(Map<String, dynamic> src) {
  final _reflection = QuadrilateralInterfaceReflection.instance;
  return QuadrilateralInterface.$all(
    quadrilateralType: src.getRequiredMapped(_reflection.quadrilateralType.oasName, (v) => 
(

    
            
                    v as String
            

)


),
    additionalProperties: AdditionalProperties(src.except(_reflection.knownKeys).map((key, v) => MapEntry(key, 
(
v

)
))),
    
  );
}

bool _$QuadrilateralInterfaceCanFromMap(Map<String, dynamic> src) {
  final _reflection = QuadrilateralInterfaceReflection.instance;
  if (!src.getOrUndefined(_reflection.quadrilateralType.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.quadrilateralType.required,
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
QuadrilateralInterface _$QuadrilateralInterfaceDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$QuadrilateralInterfaceFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$QuadrilateralInterfaceCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$QuadrilateralInterfaceCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Map<String,dynamic> _$QuadrilateralInterfaceSerialize(QuadrilateralInterface src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$QuadrilateralInterfaceToXml(QuadrilateralInterface instance) {
  final reflection = QuadrilateralInterfaceXmlReflection.instance;
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

QuadrilateralInterface _$QuadrilateralInterfaceFromXml(XmlElement src) {
  final reflection = QuadrilateralInterfaceXmlReflection.instance;
  return QuadrilateralInterface.$all(

  );
}
*/

