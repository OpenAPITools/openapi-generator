// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'outer_composite.dart';


//class serialization

Map<String, dynamic> _$OuterCompositeToMap(OuterComposite instance) {
  final _reflection = OuterCompositeReflection.instance;
  return <String, dynamic>{
    if (instance.myNumber.isDefined)
    _reflection.myNumberPart.oasName: (
            num

 v) {
      return v;
    }(instance.myNumber.valueRequired),
    if (instance.myString.isDefined)
    _reflection.myStringPart.oasName: (
            String

 v) {
      return v;
    }(instance.myString.valueRequired),
    if (instance.myBoolean.isDefined)
    _reflection.myBooleanPart.oasName: (
            bool

 v) {
      return v;
    }(instance.myBoolean.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

OuterComposite _$OuterCompositeFromMap(Map<String, dynamic> src) {
  const _reflection = OuterCompositeReflection.instance;
  return OuterComposite.$all(
    myNumber: src.getOrUndefinedMapped(_reflection.myNumberPart.oasName, (v) => 
(

            
                    ( v is num ? v as num :
num.parse(v.toString())



)

)


),
myString: src.getOrUndefinedMapped(_reflection.myStringPart.oasName, (v) => 
(

            
                    ( v is String ? v as String :




throwArgumentMismatch(String, v)

)

)


),
myBoolean: src.getOrUndefinedMapped(_reflection.myBooleanPart.oasName, (v) => 
(

            
                    ( v is bool ? v as bool :

bool.parse(v.toString())


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

bool _$OuterCompositeCanFromMap(Map<String, dynamic> src) {
  final _reflection = OuterCompositeReflection.instance;

  if (!src.getOrUndefined(_reflection.myNumberPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is num
     || (num.tryParse(v.toString()) != null)
    
    
    
)
),
    unDefined: () => !_reflection.myNumberPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.myStringPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
),
    unDefined: () => !_reflection.myStringPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.myBooleanPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is bool
    
     || (bool.tryParse(v.toString()) != null)
    
    
)
),
    unDefined: () => !_reflection.myBooleanPart.required,
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
Map<String, dynamic> _$OuterCompositeSerialize(OuterComposite src) {
  Map<String, dynamic> initialResult = () {
    
      return _$OuterCompositeToMap(src);
    
  }();
  return initialResult;
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

