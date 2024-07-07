// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'value.dart';


//class serialization

Map<String, dynamic> _$ValueToMap(Value instance) {
  final _reflection = ValueReflection.instance;
  return <String, dynamic>{
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
    if (instance.oneOf0.isDefined) ...instance.oneOf0.valueRequired.toMap(),
    
  };
}

Value _$ValueFromMap(Map<String, dynamic> src) {
  const _reflection = ValueReflection.instance;
  return Value.$all(
        additionalProperties: AdditionalProperties(src.except(_reflection.knownKeys).map((key, v) => MapEntry(key, 
(
v

)
))),
    
    oneOf0: Scalar.canDeserialize(src) ? UndefinedWrapper(Scalar.deserialize(src)) :  UndefinedWrapper.undefined(),
    oneOf1:  UndefinedWrapper.undefined(),
  );
}

bool _$ValueCanFromMap(Map<String, dynamic> src) {
  final _reflection = ValueReflection.instance;

    if (!src.except(_reflection.knownKeys).values.every((v) => v == null ? true :
(
true
))) {
    return false;
  }


  final oneOfs = [
    () => Scalar.canDeserialize(src),
    ];
  final validOneOfs = oneOfs.where((x) => x()).take(2).length;
  if (validOneOfs == 0 || validOneOfs > 1) {
    return false;
  }
  
  return true;
}

/// Deserializes a primitive Object (num, String, List, Map).
Value _$ValueDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$ValueFromMap(src);
  } else {
    
    final v = src;
    return Value.$all(
      oneOf0: (v == null ? false :
(

    
            Scalar.canDeserialize(v)
            
)) ? UndefinedWrapper(Scalar.deserialize
(

            v

)


) : UndefinedWrapper.undefined(),      oneOf1: (v == null ? false :
(

    
            
            v is List && v.every((v) => v == null ? false :
(

    
            Scalar.canDeserialize(v)
            
))
)) ? UndefinedWrapper(
(

            
            v as List
            

)

.map((v) => Scalar.deserialize
(

            v

)


).toList()
) : UndefinedWrapper.undefined(),
      // Additional Properties only make sense if the src is a Map<String, dynamic>
      additionalProperties: AdditionalProperties(),
    );
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$ValueCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$ValueCanFromMap(src);
  } else {
    final v = src;
    final oneOfs = [
      () => v == null ? false :
(

    
            Scalar.canDeserialize(v)
            
),
      () => v == null ? false :
(

    
            
            v is List && v.every((v) => v == null ? false :
(

    
            Scalar.canDeserialize(v)
            
))
),
    ];
    final validOneOfs = oneOfs.where((x) => x()).take(2).length;
    if (validOneOfs == 1) {
      return true;
    }
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Object? _$ValueSerialize(Value src) {
  Object? initialResult = () {
    
    
    if (src.oneOf0.isDefined) {final v = src.oneOf0.valueRequired; return v.serialize(); }
    if (src.oneOf1.isDefined) {final v = src.oneOf1.valueRequired; return v.map((v) => v.serialize()).toList(); }
    return null;
  }();
  if (initialResult is Map<String, Object?>) {
    return {
      ...src.additionalProperties,
      ...initialResult,
    };
  }
  return initialResult;
}


/*
XmlElement _$ValueToXml(Value instance) {
  final reflection = ValueXmlReflection.instance;
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

Value _$ValueFromXml(XmlElement src) {
  final reflection = ValueXmlReflection.instance;
  return Value.$all(

  );
}
*/

