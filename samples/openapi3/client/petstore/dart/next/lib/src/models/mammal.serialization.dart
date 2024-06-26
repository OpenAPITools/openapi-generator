// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'mammal.dart';


//class serialization

Map<String, dynamic> _$MammalToMap(Mammal instance) {
  final _reflection = MammalReflection.instance;
  return <String, dynamic>{
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
    if (instance.oneOf0.isDefined) ...instance.oneOf0.valueRequired.toMap(),
    
    if (instance.oneOf2.isDefined) ...instance.oneOf2.valueRequired.toMap(),
    
  };
}

Mammal _$MammalFromMap(Map<String, dynamic> src) {
  final _reflection = MammalReflection.instance;
  return Mammal.$all(
        additionalProperties: AdditionalProperties(src.except(_reflection.knownKeys).map((key, v) => MapEntry(key, 
(
v

)
))),
    
    oneOf0: Whale.canDeserialize(src) ? UndefinedWrapper(Whale.deserialize(src)) :  UndefinedWrapper.undefined(),
    oneOf1:  UndefinedWrapper.undefined(),
    oneOf2: Pig.canDeserialize(src) ? UndefinedWrapper(Pig.deserialize(src)) :  UndefinedWrapper.undefined(),
  );
}

bool _$MammalCanFromMap(Map<String, dynamic> src) {
  final _reflection = MammalReflection.instance;
    if (!src.except(_reflection.knownKeys).values.every((v) => v == null ? true :
(
true
))) {
    return false;
  }
  
  final oneOfs = [
    () => Whale.canDeserialize(src),
  
  
    () => Pig.canDeserialize(src),
  ];
  final validOneOfs = oneOfs.where((x) => x()).take(2).length;
  if (validOneOfs == 0 || validOneOfs > 1) {
    return false;
  }
  
  return true;
}

/// Deserializes a primitive Object (num, String, List, Map).
Mammal _$MammalDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$MammalFromMap(src);
  } else {
    
    final v = src;
    return Mammal.$all(
      oneOf0: (v == null ? false :
(

    
            Whale.canDeserialize(v)
            
)) ? UndefinedWrapper(Whale.deserialize
(

    
            v


)


) : UndefinedWrapper.undefined(),      oneOf1: (v == null ? false :
(

    
            
            v is Zebra
)) ? UndefinedWrapper(
(

    
            
                    v as Zebra
            

)


) : UndefinedWrapper.undefined(),      oneOf2: (v == null ? false :
(

    
            Pig.canDeserialize(v)
            
)) ? UndefinedWrapper(Pig.deserialize
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
bool _$MammalCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$MammalCanFromMap(src);
  } else {
    final v = src;
    final oneOfs = [
      () => v == null ? false :
(

    
            Whale.canDeserialize(v)
            
),
      () => v == null ? false :
(

    
            
            v is Zebra
),
      () => v == null ? false :
(

    
            Pig.canDeserialize(v)
            
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
Object? _$MammalSerialize(Mammal src) {
  
  
  if (src.oneOf0.isDefined) {final v = src.oneOf0.valueRequired; return v.serialize(); }
  if (src.oneOf1.isDefined) {final v = src.oneOf1.valueRequired; return v; }
  if (src.oneOf2.isDefined) {final v = src.oneOf2.valueRequired; return v.serialize(); }
  return null;
}


/*
XmlElement _$MammalToXml(Mammal instance) {
  final reflection = MammalXmlReflection.instance;
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

Mammal _$MammalFromXml(XmlElement src) {
  final reflection = MammalXmlReflection.instance;
  return Mammal.$all(

  );
}
*/

