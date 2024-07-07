// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'gm_fruit.dart';


//class serialization

Map<String, dynamic> _$GmFruitToMap(GmFruit instance) {
  final _reflection = GmFruitReflection.instance;
  return <String, dynamic>{
    if (instance.color.isDefined)
    _reflection.colorPart.oasName: (
            String
 v) {
      return v;
    }(instance.color.valueRequired),
    
    
    if (instance.anyOf0.isDefined) ...?instance.anyOf0.valueRequired?.toMap(),
    if (instance.anyOf1.isDefined) ...instance.anyOf1.valueRequired.toMap(),
  };
}

GmFruit _$GmFruitFromMap(Map<String, dynamic> src) {
  const _reflection = GmFruitReflection.instance;
  return GmFruit.$all(
    color: src.getOrUndefinedMapped(_reflection.colorPart.oasName, (v) => 
(

            
                    ( v is String ? v as String :




throwArgumentMismatch(String, v)

)

)


),
    
    
    anyOf0: Apple.canDeserialize(src) ? UndefinedWrapper(Apple.deserialize(src)) :  UndefinedWrapper.undefined(),
    anyOf1: Banana.canDeserialize(src) ? UndefinedWrapper(Banana.deserialize(src)) :  UndefinedWrapper.undefined(),
  );
}

bool _$GmFruitCanFromMap(Map<String, dynamic> src) {
  final _reflection = GmFruitReflection.instance;

  if (!src.getOrUndefined(_reflection.colorPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
),
    unDefined: () => !_reflection.colorPart.required,
)) {
    return false;
  }



  final anyOfs = [
    () => Apple.canDeserialize(src),
  
    () => Banana.canDeserialize(src),
  ];
  final validAnyOfs = anyOfs.where((x) => x()).take(1).length;
  if (validAnyOfs == 0) {
    return false;
  }
  
  return true;
}

/// Deserializes a primitive Object (num, String, List, Map).
GmFruit _$GmFruitDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$GmFruitFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$GmFruitCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$GmFruitCanFromMap(src);
  } else {
    final v = src;
    final anyOfs = [
      () => v == null ? true :
(

    
            Apple.canDeserialize(v)
            
),
      () => v == null ? false :
(

    
            Banana.canDeserialize(v)
            
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
Map<String, dynamic> _$GmFruitSerialize(GmFruit src) {
  Map<String, dynamic> initialResult = () {
    
    return src.toMap();
    
    
  }();
  return initialResult;
}


/*
XmlElement _$GmFruitToXml(GmFruit instance) {
  final reflection = GmFruitXmlReflection.instance;
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

GmFruit _$GmFruitFromXml(XmlElement src) {
  final reflection = GmFruitXmlReflection.instance;
  return GmFruit.$all(

  );
}
*/

