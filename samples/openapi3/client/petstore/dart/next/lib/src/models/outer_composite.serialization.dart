// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'outer_composite.dart';


//class serialization

Map<String, dynamic> _$OuterCompositeToMap(OuterComposite instance) {
  final _reflection = OuterCompositeReflection.instance;
  return <String, dynamic>{
    if (instance.myNumber.isDefined)
    _reflection.myNumber.oasName: (
            num
 v) {
      return v;
    }(instance.myNumber.valueRequired),
    if (instance.myString.isDefined)
    _reflection.myString.oasName: (
            String
 v) {
      return v;
    }(instance.myString.valueRequired),
    if (instance.myBoolean.isDefined)
    _reflection.myBoolean.oasName: (
            bool
 v) {
      return v;
    }(instance.myBoolean.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

OuterComposite _$OuterCompositeFromMap(Map<String, dynamic> src) {
  final _reflection = OuterCompositeReflection.instance;
  return OuterComposite.$all(
    myNumber: src.getOrUndefinedMapped(_reflection.myNumber.oasName, (v) => 
(

    
            
                    v as num
            

)


),
myString: src.getOrUndefinedMapped(_reflection.myString.oasName, (v) => 
(

    
            
                    v as String
            

)


),
myBoolean: src.getOrUndefinedMapped(_reflection.myBoolean.oasName, (v) => 
(

    
            
                    v as bool
            

)


),
    additionalProperties: AdditionalProperties(src.except(_reflection.knownKeys).map((key, v) => MapEntry(key, 
(
v

)
))),
    
  );
}

bool _$OuterCompositeCanFromMap(Map<String, dynamic> src) {
  final _reflection = OuterCompositeReflection.instance;
  if (!src.getOrUndefined(_reflection.myNumber.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is num
),
    unDefined: () => !_reflection.myNumber.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.myString.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.myString.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.myBoolean.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is bool
),
    unDefined: () => !_reflection.myBoolean.required,
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
OuterComposite _$OuterCompositeDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$OuterCompositeFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$OuterCompositeCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$OuterCompositeCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Map<String,dynamic> _$OuterCompositeSerialize(OuterComposite src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$OuterCompositeToXml(OuterComposite instance) {
  final reflection = OuterCompositeXmlReflection.instance;
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

OuterComposite _$OuterCompositeFromXml(XmlElement src) {
  final reflection = OuterCompositeXmlReflection.instance;
  return OuterComposite.$all(

  );
}
*/

