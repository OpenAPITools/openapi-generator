// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'tag.dart';


//class serialization

Map<String, dynamic> _$TagToMap(Tag instance) {
  final _reflection = TagReflection.instance;
  return <String, dynamic>{
    if (instance.id.isDefined)
    _reflection.id.oasName: (
            int
 v) {
      return v;
    }(instance.id.valueRequired),
    if (instance.name.isDefined)
    _reflection.name.oasName: (
            String
 v) {
      return v;
    }(instance.name.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

Tag _$TagFromMap(Map<String, dynamic> src) {
  final _reflection = TagReflection.instance;
  return Tag.$all(
    id: src.getOrUndefinedMapped(_reflection.id.oasName, (v) => 
(

    
            
                    v as int
            

)


),
name: src.getOrUndefinedMapped(_reflection.name.oasName, (v) => 
(

    
            
                    v as String
            

)


),
    additionalProperties: AdditionalProperties(src.except(_reflection.knownKeys).map((key, v) => MapEntry(key, 
(
v

)
))),
    
  );
}

bool _$TagCanFromMap(Map<String, dynamic> src) {
  final _reflection = TagReflection.instance;
  if (!src.getOrUndefined(_reflection.id.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is int
),
    unDefined: () => !_reflection.id.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.name.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.name.required,
)) {
    return false;
  }
  if (!src.except(_reflection.knownKeys).values.every((v) => v == null ? true :
(
true
))) {
    return false;
  }
  
  return true;
}

/// Deserializes a primitive Object (num, String, List, Map).
Tag _$TagDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$TagFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$TagCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$TagCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Map<String,dynamic> _$TagSerialize(Tag src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$TagToXml(Tag instance) {
  final reflection = TagXmlReflection.instance;
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

Tag _$TagFromXml(XmlElement src) {
  final reflection = TagXmlReflection.instance;
  return Tag.$all(

  );
}
*/

