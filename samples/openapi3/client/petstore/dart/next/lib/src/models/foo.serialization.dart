// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'foo.dart';


//class serialization

Map<String, dynamic> _$FooToMap(Foo instance) {
  final _reflection = FooReflection.instance;
  return <String, dynamic>{
    if (instance.bar.isDefined)
    _reflection.bar.oasName: (
            String
 v) {
      return v;
    }(instance.bar.valueRequired),
    
    
  };
}

Foo _$FooFromMap(Map<String, dynamic> src) {
  final _reflection = FooReflection.instance;
  return Foo.$all(
    bar: src.getOrUndefinedMapped(_reflection.bar.oasName, (v) => 
(

    
            
                    v as String
            

)


),
    
    
  );
}

bool _$FooCanFromMap(Map<String, dynamic> src) {
  final _reflection = FooReflection.instance;
  if (!src.getOrUndefined(_reflection.bar.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.bar.required,
)) {
    return false;
  }
  
  return true;
}

/// Deserializes a primitive Object (num, String, List, Map).
Foo _$FooDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$FooFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$FooCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$FooCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Object? _$FooSerialize(Foo src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$FooToXml(Foo instance) {
  final reflection = FooXmlReflection.instance;
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

Foo _$FooFromXml(XmlElement src) {
  final reflection = FooXmlReflection.instance;
  return Foo.$all(

  );
}
*/

