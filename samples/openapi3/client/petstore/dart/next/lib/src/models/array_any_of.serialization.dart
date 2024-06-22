// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'array_any_of.dart';


//class serialization

Map<String, dynamic> _$ArrayAnyOfToMap(ArrayAnyOf instance) {
  final _reflection = ArrayAnyOfReflection.instance;
  return <String, dynamic>{
    
    
  };
}

ArrayAnyOf _$ArrayAnyOfFromMap(Map<String, dynamic> src) {
  final _reflection = ArrayAnyOfReflection.instance;
  return ArrayAnyOf.$all(
        
    
    anyOf0:  UndefinedWrapper.undefined(),
    anyOf1:  UndefinedWrapper.undefined(),
  );
}

bool _$ArrayAnyOfCanFromMap(Map<String, dynamic> src) {
  final _reflection = ArrayAnyOfReflection.instance;
    
  final anyOfs = [
  
  ];
  final validAnyOfs = anyOfs.where((x) => x()).take(1).length;
  if (validAnyOfs == 0) {
    return false;
  }
  
  return true;
}

/// Deserializes a primitive Object (num, String, List, Map).
ArrayAnyOf _$ArrayAnyOfDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$ArrayAnyOfFromMap(src);
  } else {
    
    final v = src;
    return ArrayAnyOf.$all(

      anyOf0: (v == null ? false :
(

    
            
            v is int
)) ? UndefinedWrapper(
(

    
            
                    v as int
            

)


) : UndefinedWrapper.undefined(),
      anyOf1: (v == null ? false :
(

    
            
            v is List && v.every((v) => v == null ? false :
(

    
            
            v is String
))
)) ? UndefinedWrapper(
(

    
            
            v as List
            

)

.map((v) => 
(

    
            
                    v as String
            

)


).toList()
) : UndefinedWrapper.undefined(),
    );
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$ArrayAnyOfCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$ArrayAnyOfCanFromMap(src);
  } else {
    final v = src;
    final anyOfs = [
      () => v == null ? false :
(

    
            
            v is int
),
      () => v == null ? false :
(

    
            
            v is List && v.every((v) => v == null ? false :
(

    
            
            v is String
))
),
    ];
    final validAnyOfs = anyOfs.where((x) => x()).take(1).length;
    if (validAnyOfs > 0) {
      return true;
    }
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Object? _$ArrayAnyOfSerialize(ArrayAnyOf src) {
  
  
  if (src.anyOf0.isDefined) {final v = src.anyOf0.valueRequired; return v; }
  
  if (src.anyOf1.isDefined) {final v = src.anyOf1.valueRequired; return v.map((v) => v).toList(); }
  
  return null;
}


/*
XmlElement _$ArrayAnyOfToXml(ArrayAnyOf instance) {
  final reflection = ArrayAnyOfXmlReflection.instance;
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

ArrayAnyOf _$ArrayAnyOfFromXml(XmlElement src) {
  final reflection = ArrayAnyOfXmlReflection.instance;
  return ArrayAnyOf.$all(

  );
}
*/

