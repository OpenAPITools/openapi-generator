// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'scalar.dart';


//class serialization

Map<String, dynamic> _$ScalarToMap(Scalar instance) {
  final _reflection = ScalarReflection.instance;
  return <String, dynamic>{
    
    
  };
}

Scalar _$ScalarFromMap(Map<String, dynamic> src) {
  final _reflection = ScalarReflection.instance;
  return Scalar.$all(
        
    
    oneOf0:  UndefinedWrapper.undefined(),
    oneOf1:  UndefinedWrapper.undefined(),
    oneOf2:  UndefinedWrapper.undefined(),
  );
}

bool _$ScalarCanFromMap(Map<String, dynamic> src) {
  final _reflection = ScalarReflection.instance;
    
  final oneOfs = [
  
  
  ];
  final validOneOfs = oneOfs.where((x) => x()).take(2).length;
  if (validOneOfs == 0 || validOneOfs > 1) {
    return false;
  }
  
  return true;
}

/// Deserializes a primitive Object (num, String, List, Map).
Scalar _$ScalarDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$ScalarFromMap(src);
  } else {
    
    final v = src;
    return Scalar.$all(
      oneOf0: (v == null ? false :
(

    
            
            v is String
)) ? UndefinedWrapper(
(

    
            
                    v as String
            

)


) : UndefinedWrapper.undefined(),      oneOf1: (v == null ? false :
(

    
            
            v is num
)) ? UndefinedWrapper(
(

    
            
                    v as num
            

)


) : UndefinedWrapper.undefined(),      oneOf2: (v == null ? false :
(

    
            
            v is bool
)) ? UndefinedWrapper(
(

    
            
                    v as bool
            

)


) : UndefinedWrapper.undefined(),
    );
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$ScalarCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$ScalarCanFromMap(src);
  } else {
    final v = src;
    final oneOfs = [
      () => v == null ? false :
(

    
            
            v is String
),
      () => v == null ? false :
(

    
            
            v is num
),
      () => v == null ? false :
(

    
            
            v is bool
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
Object? _$ScalarSerialize(Scalar src) {
  
  
  if (src.oneOf0.isDefined) {final v = src.oneOf0.valueRequired; return v; }
  if (src.oneOf1.isDefined) {final v = src.oneOf1.valueRequired; return v; }
  if (src.oneOf2.isDefined) {final v = src.oneOf2.valueRequired; return v; }
  return null;
}


/*
XmlElement _$ScalarToXml(Scalar instance) {
  final reflection = ScalarXmlReflection.instance;
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

Scalar _$ScalarFromXml(XmlElement src) {
  final reflection = ScalarXmlReflection.instance;
  return Scalar.$all(

  );
}
*/

