// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'pig.dart';


//class serialization

Map<String, dynamic> _$PigToMap(Pig instance) {
  final _reflection = PigReflection.instance;
  return <String, dynamic>{
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
    if (instance.oneOf0.isDefined) ...instance.oneOf0.valueRequired.toMap(),
    
    if (instance.oneOf1.isDefined) ...instance.oneOf1.valueRequired.toMap(),
    
  };
}

Pig _$PigFromMap(Map<String, dynamic> src) {
  final _reflection = PigReflection.instance;
  return Pig.$all(
        additionalProperties: AdditionalProperties(src.except(_reflection.knownKeys).map((key, v) => MapEntry(key, 
(
v

)
))),
    
    oneOf0: BasquePig.canDeserialize(src) ? UndefinedWrapper(BasquePig.deserialize(src)) :  UndefinedWrapper.undefined(),
    oneOf1: DanishPig.canDeserialize(src) ? UndefinedWrapper(DanishPig.deserialize(src)) :  UndefinedWrapper.undefined(),
  );
}

bool _$PigCanFromMap(Map<String, dynamic> src) {
  final _reflection = PigReflection.instance;
    if (!src.except(_reflection.knownKeys).values.every((v) => v == null ? true :
(
true
))) {
    return false;
  }
  
  final oneOfs = [
    () => BasquePig.canDeserialize(src),
  
    () => DanishPig.canDeserialize(src),
  ];
  final validOneOfs = oneOfs.where((x) => x()).take(2).length;
  if (validOneOfs == 0 || validOneOfs > 1) {
    return false;
  }
  
  return true;
}

/// Deserializes a primitive Object (num, String, List, Map).
Pig _$PigDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$PigFromMap(src);
  } else {
    
    final v = src;
    return Pig.$all(
      oneOf0: (v == null ? false :
(

    
            BasquePig.canDeserialize(v)
            
)) ? UndefinedWrapper(BasquePig.deserialize
(

    
            v


)


) : UndefinedWrapper.undefined(),      oneOf1: (v == null ? false :
(

    
            DanishPig.canDeserialize(v)
            
)) ? UndefinedWrapper(DanishPig.deserialize
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
bool _$PigCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$PigCanFromMap(src);
  } else {
    final v = src;
    final oneOfs = [
      () => v == null ? false :
(

    
            BasquePig.canDeserialize(v)
            
),
      () => v == null ? false :
(

    
            DanishPig.canDeserialize(v)
            
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
Object? _$PigSerialize(Pig src) {
  
  
  if (src.oneOf0.isDefined) {final v = src.oneOf0.valueRequired; return v.serialize(); }
  if (src.oneOf1.isDefined) {final v = src.oneOf1.valueRequired; return v.serialize(); }
  return null;
}


/*
XmlElement _$PigToXml(Pig instance) {
  final reflection = PigXmlReflection.instance;
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

Pig _$PigFromXml(XmlElement src) {
  final reflection = PigXmlReflection.instance;
  return Pig.$all(

  );
}
*/

