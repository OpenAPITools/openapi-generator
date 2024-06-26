// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'array_test.dart';


//class serialization

Map<String, dynamic> _$ArrayTestToMap(ArrayTest instance) {
  final _reflection = ArrayTestReflection.instance;
  return <String, dynamic>{
    if (instance.arrayOfString.isDefined)
    _reflection.arrayOfString.oasName: (
    List<
        
            String
>
 v) {
      return v.map((v) => v).toList();
    }(instance.arrayOfString.valueRequired),
    if (instance.arrayArrayOfInteger.isDefined)
    _reflection.arrayArrayOfInteger.oasName: (
    List<
        
    List<
        
            int
>
>
 v) {
      return v.map((v) => v.map((v) => v).toList()).toList();
    }(instance.arrayArrayOfInteger.valueRequired),
    if (instance.arrayArrayOfModel.isDefined)
    _reflection.arrayArrayOfModel.oasName: (
    List<
        
    List<
        
            ReadOnlyFirst
>
>
 v) {
      return v.map((v) => v.map((v) => v.serialize()).toList()).toList();
    }(instance.arrayArrayOfModel.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

ArrayTest _$ArrayTestFromMap(Map<String, dynamic> src) {
  final _reflection = ArrayTestReflection.instance;
  return ArrayTest.$all(
    arrayOfString: src.getOrUndefinedMapped(_reflection.arrayOfString.oasName, (v) => 
(

    
            
            v as List
            

)

.map((v) => 
(

    
            
                    v as String
            

)


).toList()
),
arrayArrayOfInteger: src.getOrUndefinedMapped(_reflection.arrayArrayOfInteger.oasName, (v) => 
(

    
            
            v as List
            

)

.map((v) => 
(

    
            
            v as List
            

)

.map((v) => 
(

    
            
                    v as int
            

)


).toList()
).toList()
),
arrayArrayOfModel: src.getOrUndefinedMapped(_reflection.arrayArrayOfModel.oasName, (v) => 
(

    
            
            v as List
            

)

.map((v) => 
(

    
            
            v as List
            

)

.map((v) => ReadOnlyFirst.deserialize
(

    
            v


)


).toList()
).toList()
),
    additionalProperties: AdditionalProperties(src.except(_reflection.knownKeys).map((key, v) => MapEntry(key, 
(
v

)
))),
    
  );
}

bool _$ArrayTestCanFromMap(Map<String, dynamic> src) {
  final _reflection = ArrayTestReflection.instance;
  if (!src.getOrUndefined(_reflection.arrayOfString.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is List && v.every((v) => v == null ? false :
(

    
            
            v is String
))
),
    unDefined: () => !_reflection.arrayOfString.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.arrayArrayOfInteger.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is List && v.every((v) => v == null ? false :
(

    
            
            v is List && v.every((v) => v == null ? false :
(

    
            
            v is int
))
))
),
    unDefined: () => !_reflection.arrayArrayOfInteger.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.arrayArrayOfModel.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is List && v.every((v) => v == null ? false :
(

    
            
            v is List && v.every((v) => v == null ? false :
(

    
            ReadOnlyFirst.canDeserialize(v)
            
))
))
),
    unDefined: () => !_reflection.arrayArrayOfModel.required,
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
ArrayTest _$ArrayTestDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$ArrayTestFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$ArrayTestCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$ArrayTestCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Object? _$ArrayTestSerialize(ArrayTest src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$ArrayTestToXml(ArrayTest instance) {
  final reflection = ArrayTestXmlReflection.instance;
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

ArrayTest _$ArrayTestFromXml(XmlElement src) {
  final reflection = ArrayTestXmlReflection.instance;
  return ArrayTest.$all(

  );
}
*/

