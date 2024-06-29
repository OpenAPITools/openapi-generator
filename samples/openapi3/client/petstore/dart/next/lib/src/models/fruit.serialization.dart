// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'fruit.dart';


//class serialization

Map<String, dynamic> _$FruitToMap(Fruit instance) {
  final _reflection = FruitReflection.instance;
  return <String, dynamic>{
    if (instance.color.isDefined)
    _reflection.color.oasName: (
            String
 v) {
      return v;
    }(instance.color.valueRequired),
    
    
    if (instance.oneOf0.isDefined) ...?instance.oneOf0.valueRequired?.toMap(),
    
    if (instance.oneOf1.isDefined) ...instance.oneOf1.valueRequired.toMap(),
    
  };
}

Fruit _$FruitFromMap(Map<String, dynamic> src) {
  final _reflection = FruitReflection.instance;
  return Fruit.$all(
    color: src.getOrUndefinedMapped(_reflection.color.oasName, (v) => 
(

    
            
                    v as String
            

)


),
    
    
    oneOf0: Apple.canDeserialize(src) ? UndefinedWrapper(Apple.deserialize(src)) :  UndefinedWrapper.undefined(),
    oneOf1: Banana.canDeserialize(src) ? UndefinedWrapper(Banana.deserialize(src)) :  UndefinedWrapper.undefined(),
  );
}

bool _$FruitCanFromMap(Map<String, dynamic> src) {
  final _reflection = FruitReflection.instance;
  if (!src.getOrUndefined(_reflection.color.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.color.required,
)) {
    return false;
  }
  
  final oneOfs = [
    () => Apple.canDeserialize(src),
  
    () => Banana.canDeserialize(src),
  ];
  final validOneOfs = oneOfs.where((x) => x()).take(2).length;
  if (validOneOfs == 0 || validOneOfs > 1) {
    return false;
  }
  
  return true;
}

/// Deserializes a primitive Object (num, String, List, Map).
Fruit _$FruitDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$FruitFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$FruitCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$FruitCanFromMap(src);
  } else {
    final v = src;
    final oneOfs = [
      () => v == null ? true :
(

    
            Apple.canDeserialize(v)
            
),
      () => v == null ? false :
(

    
            Banana.canDeserialize(v)
            
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
Map<String,dynamic> _$FruitSerialize(Fruit src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$FruitToXml(Fruit instance) {
  final reflection = FruitXmlReflection.instance;
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

Fruit _$FruitFromXml(XmlElement src) {
  final reflection = FruitXmlReflection.instance;
  return Fruit.$all(

  );
}
*/

