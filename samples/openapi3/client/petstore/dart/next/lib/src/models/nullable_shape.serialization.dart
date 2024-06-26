// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'nullable_shape.dart';


//class serialization

Map<String, dynamic> _$NullableShapeToMap(NullableShape instance) {
  final _reflection = NullableShapeReflection.instance;
  return <String, dynamic>{
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
    if (instance.oneOf0.isDefined) ...instance.oneOf0.valueRequired.toMap(),
    
    if (instance.oneOf1.isDefined) ...instance.oneOf1.valueRequired.toMap(),
    
  };
}

NullableShape _$NullableShapeFromMap(Map<String, dynamic> src) {
  final _reflection = NullableShapeReflection.instance;
  return NullableShape.$all(
        additionalProperties: AdditionalProperties(src.except(_reflection.knownKeys).map((key, v) => MapEntry(key, 
(
v

)
))),
    
    oneOf0: Triangle.canDeserialize(src) ? UndefinedWrapper(Triangle.deserialize(src)) :  UndefinedWrapper.undefined(),
    oneOf1: Quadrilateral.canDeserialize(src) ? UndefinedWrapper(Quadrilateral.deserialize(src)) :  UndefinedWrapper.undefined(),
  );
}

bool _$NullableShapeCanFromMap(Map<String, dynamic> src) {
  final _reflection = NullableShapeReflection.instance;
    if (!src.except(_reflection.knownKeys).values.every((v) => v == null ? true :
(
true
))) {
    return false;
  }
  
  final oneOfs = [
    () => Triangle.canDeserialize(src),
  
    () => Quadrilateral.canDeserialize(src),
  ];
  final validOneOfs = oneOfs.where((x) => x()).take(2).length;
  if (validOneOfs == 0 || validOneOfs > 1) {
    return false;
  }
  
  return true;
}

/// Deserializes a primitive Object (num, String, List, Map).
NullableShape _$NullableShapeDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$NullableShapeFromMap(src);
  } else {
    
    final v = src;
    return NullableShape.$all(
      oneOf0: (v == null ? false :
(

    
            Triangle.canDeserialize(v)
            
)) ? UndefinedWrapper(Triangle.deserialize
(

    
            v


)


) : UndefinedWrapper.undefined(),      oneOf1: (v == null ? false :
(

    
            Quadrilateral.canDeserialize(v)
            
)) ? UndefinedWrapper(Quadrilateral.deserialize
(

    
            v


)


) : UndefinedWrapper.undefined(),
      // Additional Properties only make sense if the src is a Map<String, dynamic>
      additionalProperties: AdditionalProperties(),
    );
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$NullableShapeCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$NullableShapeCanFromMap(src);
  } else {
    final v = src;
    final oneOfs = [
      () => v == null ? false :
(

    
            Triangle.canDeserialize(v)
            
),
      () => v == null ? false :
(

    
            Quadrilateral.canDeserialize(v)
            
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
Object? _$NullableShapeSerialize(NullableShape src) {
  
  
  if (src.oneOf0.isDefined) {final v = src.oneOf0.valueRequired; return v.serialize(); }
  if (src.oneOf1.isDefined) {final v = src.oneOf1.valueRequired; return v.serialize(); }
  return null;
}


/*
XmlElement _$NullableShapeToXml(NullableShape instance) {
  final reflection = NullableShapeXmlReflection.instance;
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

NullableShape _$NullableShapeFromXml(XmlElement src) {
  final reflection = NullableShapeXmlReflection.instance;
  return NullableShape.$all(

  );
}
*/

