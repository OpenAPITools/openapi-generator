// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'fake_one_of_w_ith_same_erasure_get200_response.dart';


//class serialization

Map<String, dynamic> _$FakeOneOfWIthSameErasureGet200ResponseToMap(FakeOneOfWIthSameErasureGet200Response instance) {
  final _reflection = FakeOneOfWIthSameErasureGet200ResponseReflection.instance;
  return <String, dynamic>{
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

FakeOneOfWIthSameErasureGet200Response _$FakeOneOfWIthSameErasureGet200ResponseFromMap(Map<String, dynamic> src) {
  final _reflection = FakeOneOfWIthSameErasureGet200ResponseReflection.instance;
  return FakeOneOfWIthSameErasureGet200Response.$all(
        additionalProperties: AdditionalProperties(src.except(_reflection.knownKeys).map((key, v) => MapEntry(key, 
(
v

)
))),
    
    oneOf0:  UndefinedWrapper.undefined(),
    oneOf1:  UndefinedWrapper.undefined(),
  );
}

bool _$FakeOneOfWIthSameErasureGet200ResponseCanFromMap(Map<String, dynamic> src) {
  final _reflection = FakeOneOfWIthSameErasureGet200ResponseReflection.instance;
    if (!src.except(_reflection.knownKeys).values.every((v) => v == null ? true :
(
true
))) {
    return false;
  }
  
  final oneOfs = [
  
  ];
  final validOneOfs = oneOfs.where((x) => x()).take(2).length;
  if (validOneOfs == 0 || validOneOfs > 1) {
    return false;
  }
  
  return true;
}

/// Deserializes a primitive Object (num, String, List, Map).
FakeOneOfWIthSameErasureGet200Response _$FakeOneOfWIthSameErasureGet200ResponseDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$FakeOneOfWIthSameErasureGet200ResponseFromMap(src);
  } else {
    
    final v = src;
    return FakeOneOfWIthSameErasureGet200Response.$all(
      oneOf0: (v == null ? false :
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
) : UndefinedWrapper.undefined(),      oneOf1: (v == null ? false :
(

    
            
            v is List && v.every((v) => v == null ? false :
(

    
            
            v is int
))
)) ? UndefinedWrapper(
(

    
            
            v as List
            

)

.map((v) => 
(

    
            
                    v as int
            

)


).toList()
) : UndefinedWrapper.undefined(),
      // Additional Properties only make sense if the src is a Map<String, dynamic>
      additionalProperties: AdditionalProperties(),
    );
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$FakeOneOfWIthSameErasureGet200ResponseCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$FakeOneOfWIthSameErasureGet200ResponseCanFromMap(src);
  } else {
    final v = src;
    final oneOfs = [
      () => v == null ? false :
(

    
            
            v is List && v.every((v) => v == null ? false :
(

    
            
            v is String
))
),
      () => v == null ? false :
(

    
            
            v is List && v.every((v) => v == null ? false :
(

    
            
            v is int
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
Object? _$FakeOneOfWIthSameErasureGet200ResponseSerialize(FakeOneOfWIthSameErasureGet200Response src) {
  
  
  if (src.oneOf0.isDefined) {final v = src.oneOf0.valueRequired; return v.map((v) => v).toList(); }
  if (src.oneOf1.isDefined) {final v = src.oneOf1.valueRequired; return v.map((v) => v).toList(); }
  return null;
}


/*
XmlElement _$FakeOneOfWIthSameErasureGet200ResponseToXml(FakeOneOfWIthSameErasureGet200Response instance) {
  final reflection = FakeOneOfWIthSameErasureGet200ResponseXmlReflection.instance;
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

FakeOneOfWIthSameErasureGet200Response _$FakeOneOfWIthSameErasureGet200ResponseFromXml(XmlElement src) {
  final reflection = FakeOneOfWIthSameErasureGet200ResponseXmlReflection.instance;
  return FakeOneOfWIthSameErasureGet200Response.$all(

  );
}
*/

