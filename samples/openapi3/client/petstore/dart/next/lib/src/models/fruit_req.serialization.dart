// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'fruit_req.dart';


//class serialization

Map<String, dynamic> _$FruitReqToMap(FruitReq instance) {
  final _reflection = FruitReqReflection.instance;
  return <String, dynamic>{
    
    
    if (instance.oneOf0.isDefined) ...instance.oneOf0.valueRequired.toMap(),
    
    if (instance.oneOf1.isDefined) ...instance.oneOf1.valueRequired.toMap(),
    
  };
}

FruitReq _$FruitReqFromMap(Map<String, dynamic> src) {
  const _reflection = FruitReqReflection.instance;
  return FruitReq.$all(
        
    
    oneOf0: AppleReq.canDeserialize(src) ? UndefinedWrapper(AppleReq.deserialize(src)) :  UndefinedWrapper.undefined(),
    oneOf1: BananaReq.canDeserialize(src) ? UndefinedWrapper(BananaReq.deserialize(src)) :  UndefinedWrapper.undefined(),
  );
}

bool _$FruitReqCanFromMap(Map<String, dynamic> src) {
  final _reflection = FruitReqReflection.instance;

  

  final oneOfs = [
    () => AppleReq.canDeserialize(src),
      () => BananaReq.canDeserialize(src),
  ];
  final validOneOfs = oneOfs.where((x) => x()).take(2).length;
  if (validOneOfs == 0 || validOneOfs > 1) {
    return false;
  }
  
  return true;
}

/// Deserializes a primitive Object (num, String, List, Map).
FruitReq _$FruitReqDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$FruitReqFromMap(src);
  } else {
    
    final v = src;
    return FruitReq.$all(
      oneOf0: (v == null ? false :
(

    
            AppleReq.canDeserialize(v)
            
)) ? UndefinedWrapper(AppleReq.deserialize
(

            v

)


) : UndefinedWrapper.undefined(),      oneOf1: (v == null ? false :
(

    
            BananaReq.canDeserialize(v)
            
)) ? UndefinedWrapper(BananaReq.deserialize
(

            v

)


) : UndefinedWrapper.undefined(),
      // Additional Properties only make sense if the src is a Map<String, dynamic>
      
    );
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$FruitReqCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$FruitReqCanFromMap(src);
  } else {
    final v = src;
    final oneOfs = [
      () => v == null ? false :
(

    
            AppleReq.canDeserialize(v)
            
),
      () => v == null ? false :
(

    
            BananaReq.canDeserialize(v)
            
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
Object? _$FruitReqSerialize(FruitReq src) {
  Object? initialResult = () {
    
    
    if (src.oneOf0.isDefined) {final v = src.oneOf0.valueRequired; return v.serialize(); }
    if (src.oneOf1.isDefined) {final v = src.oneOf1.valueRequired; return v.serialize(); }
    return null;
  }();
  return initialResult;
}


/*
XmlElement _$FruitReqToXml(FruitReq instance) {
  final reflection = FruitReqXmlReflection.instance;
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

FruitReq _$FruitReqFromXml(XmlElement src) {
  final reflection = FruitReqXmlReflection.instance;
  return FruitReq.$all(

  );
}
*/

