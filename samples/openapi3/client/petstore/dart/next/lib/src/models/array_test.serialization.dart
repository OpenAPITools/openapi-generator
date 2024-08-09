// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'array_test.dart';


//class serialization

Map<String, dynamic> _$ArrayTestToMap(ArrayTest instance) {
  final _reflection = ArrayTestReflection.instance;
  return <String, dynamic>{
    if (instance.arrayOfString.isDefined)
    _reflection.arrayOfStringPart.oasName: (
    List<
        
            String

>

 v) {
      return v.map((v) => v).toList();
    }(instance.arrayOfString.valueRequired),
    if (instance.arrayArrayOfInteger.isDefined)
    _reflection.arrayArrayOfIntegerPart.oasName: (
    List<
        
    List<
        
            int

>

>

 v) {
      return v.map((v) => v.map((v) => v).toList()).toList();
    }(instance.arrayArrayOfInteger.valueRequired),
    if (instance.arrayArrayOfModel.isDefined)
    _reflection.arrayArrayOfModelPart.oasName: (
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
  const _reflection = ArrayTestReflection.instance;
  return ArrayTest.$all(
    arrayOfString: src.getOrUndefinedMapped(_reflection.arrayOfStringPart.oasName, (v) => 
(

            
            v as List
            

)

.map((v) => 
(

            
                    ( v is String ? v as String :




throwArgumentMismatch(String, v)

)

)


).toList()
),
arrayArrayOfInteger: src.getOrUndefinedMapped(_reflection.arrayArrayOfIntegerPart.oasName, (v) => 
(

            
            v as List
            

)

.map((v) => 
(

            
            v as List
            

)

.map((v) => 
(

            
                    ( v is int ? v as int :
int.parse(v.toString())



)

)


).toList()
).toList()
),
arrayArrayOfModel: src.getOrUndefinedMapped(_reflection.arrayArrayOfModelPart.oasName, (v) => 
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

  if (!src.getOrUndefined(_reflection.arrayOfStringPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is List && v.every((v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
))
),
    unDefined: () => !_reflection.arrayOfStringPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.arrayArrayOfIntegerPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is List && v.every((v) => v == null ? false :
(

    
            
            v is List && v.every((v) => v == null ? false :
(

    
            
            (v is int
     || (int.tryParse(v.toString()) != null)
    
    
    
)
))
))
),
    unDefined: () => !_reflection.arrayArrayOfIntegerPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.arrayArrayOfModelPart.oasName).split<bool>(
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
    unDefined: () => !_reflection.arrayArrayOfModelPart.required,
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
Map<String, dynamic> _$ArrayTestSerialize(ArrayTest src) {
  Map<String, dynamic> initialResult = () {
    
      return _$ArrayTestToMap(src);
    
  }();
  return initialResult;
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

