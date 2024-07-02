// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'nullable_class.dart';


//class serialization

Map<String, dynamic> _$NullableClassToMap(NullableClass instance) {
  final _reflection = NullableClassReflection.instance;
  return <String, dynamic>{
    if (instance.integerProp.isDefined)
    _reflection.integerProp.oasName: (
            int
? v) {
      return v;
    }(instance.integerProp.valueRequired),
    if (instance.numberProp.isDefined)
    _reflection.numberProp.oasName: (
            num
? v) {
      return v;
    }(instance.numberProp.valueRequired),
    if (instance.booleanProp.isDefined)
    _reflection.booleanProp.oasName: (
            bool
? v) {
      return v;
    }(instance.booleanProp.valueRequired),
    if (instance.stringProp.isDefined)
    _reflection.stringProp.oasName: (
            String
? v) {
      return v;
    }(instance.stringProp.valueRequired),
    if (instance.dateProp.isDefined)
    _reflection.dateProp.oasName: (
            DateTime
? v) {
      return v;
    }(instance.dateProp.valueRequired),
    if (instance.datetimeProp.isDefined)
    _reflection.datetimeProp.oasName: (
            DateTime
? v) {
      return v;
    }(instance.datetimeProp.valueRequired),
    if (instance.arrayNullableProp.isDefined)
    _reflection.arrayNullableProp.oasName: (
    List<
        
            Map<String, Object?>
>
? v) {
      return v?.map((v) => v).toList();
    }(instance.arrayNullableProp.valueRequired),
    if (instance.arrayAndItemsNullableProp.isDefined)
    _reflection.arrayAndItemsNullableProp.oasName: (
    List<
        
            Map<String, Object?>
?>
? v) {
      return v?.map((v) => v).toList();
    }(instance.arrayAndItemsNullableProp.valueRequired),
    if (instance.arrayItemsNullable.isDefined)
    _reflection.arrayItemsNullable.oasName: (
    List<
        
            Map<String, Object?>
?>
 v) {
      return v.map((v) => v).toList();
    }(instance.arrayItemsNullable.valueRequired),
    if (instance.objectNullableProp.isDefined)
    _reflection.objectNullableProp.oasName: (
    Map<String, 
        
            Map<String, Object?>
>
? v) {
      return v;
    }(instance.objectNullableProp.valueRequired),
    if (instance.objectAndItemsNullableProp.isDefined)
    _reflection.objectAndItemsNullableProp.oasName: (
    Map<String, 
        
            Map<String, Object?>
?>
? v) {
      return v;
    }(instance.objectAndItemsNullableProp.valueRequired),
    if (instance.objectItemsNullable.isDefined)
    _reflection.objectItemsNullable.oasName: (
    Map<String, 
        
            Map<String, Object?>
?>
 v) {
      return v;
    }(instance.objectItemsNullable.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

NullableClass _$NullableClassFromMap(Map<String, dynamic> src) {
  final _reflection = NullableClassReflection.instance;
  return NullableClass.$all(
    integerProp: src.getOrUndefinedMapped(_reflection.integerProp.oasName, (v) => 
(

    
            
                    v as int
            ?

)


),
numberProp: src.getOrUndefinedMapped(_reflection.numberProp.oasName, (v) => 
(

    
            
                    v as num
            ?

)


),
booleanProp: src.getOrUndefinedMapped(_reflection.booleanProp.oasName, (v) => 
(

    
            
                    v as bool
            ?

)


),
stringProp: src.getOrUndefinedMapped(_reflection.stringProp.oasName, (v) => 
(

    
            
                    v as String
            ?

)


),
dateProp: src.getOrUndefinedMapped(_reflection.dateProp.oasName, (v) => 
(

    
            
                    v as DateTime
            ?

)


),
datetimeProp: src.getOrUndefinedMapped(_reflection.datetimeProp.oasName, (v) => 
(

    
            
                    v as DateTime
            ?

)


),
arrayNullableProp: src.getOrUndefinedMapped(_reflection.arrayNullableProp.oasName, (v) => 
(

    
            
            v as List
            ?

)

?.map((v) => 
(

    v as Map<String, dynamic>

)


).toList()
),
arrayAndItemsNullableProp: src.getOrUndefinedMapped(_reflection.arrayAndItemsNullableProp.oasName, (v) => 
(

    
            
            v as List
            ?

)

?.map((v) => 
(

    v as Map<String, dynamic>?

)


).toList()
),
arrayItemsNullable: src.getOrUndefinedMapped(_reflection.arrayItemsNullable.oasName, (v) => 
(

    
            
            v as List
            

)

.map((v) => 
(

    v as Map<String, dynamic>?

)


).toList()
),
objectNullableProp: src.getOrUndefinedMapped(_reflection.objectNullableProp.oasName, (v) => 
(

    v as Map<String, dynamic>?

)
?.map((k,v) => MapEntry(k, 
(

    v as Map<String, dynamic>

)


))

),
objectAndItemsNullableProp: src.getOrUndefinedMapped(_reflection.objectAndItemsNullableProp.oasName, (v) => 
(

    v as Map<String, dynamic>?

)
?.map((k,v) => MapEntry(k, 
(

    v as Map<String, dynamic>?

)


))

),
objectItemsNullable: src.getOrUndefinedMapped(_reflection.objectItemsNullable.oasName, (v) => 
(

    v as Map<String, dynamic>

)
.map((k,v) => MapEntry(k, 
(

    v as Map<String, dynamic>?

)


))

),
    additionalProperties: AdditionalProperties(src.except(_reflection.knownKeys).map((key, v) => MapEntry(key, 
(

    v as Map<String, dynamic>?

)


))),
    
  );
}

bool _$NullableClassCanFromMap(Map<String, dynamic> src) {
  final _reflection = NullableClassReflection.instance;
  if (!src.getOrUndefined(_reflection.integerProp.oasName).split<bool>(
    defined: (v) => v == null ? true :
(

    
            
            v is int
),
    unDefined: () => !_reflection.integerProp.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.numberProp.oasName).split<bool>(
    defined: (v) => v == null ? true :
(

    
            
            v is num
),
    unDefined: () => !_reflection.numberProp.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.booleanProp.oasName).split<bool>(
    defined: (v) => v == null ? true :
(

    
            
            v is bool
),
    unDefined: () => !_reflection.booleanProp.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.stringProp.oasName).split<bool>(
    defined: (v) => v == null ? true :
(

    
            
            v is String
),
    unDefined: () => !_reflection.stringProp.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.dateProp.oasName).split<bool>(
    defined: (v) => v == null ? true :
(

    
            
            v is DateTime
),
    unDefined: () => !_reflection.dateProp.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.datetimeProp.oasName).split<bool>(
    defined: (v) => v == null ? true :
(

    
            
            v is DateTime
),
    unDefined: () => !_reflection.datetimeProp.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.arrayNullableProp.oasName).split<bool>(
    defined: (v) => v == null ? true :
(

    
            
            v is List && v.every((v) => v == null ? false :
(

    v is Map<String, dynamic>
))
),
    unDefined: () => !_reflection.arrayNullableProp.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.arrayAndItemsNullableProp.oasName).split<bool>(
    defined: (v) => v == null ? true :
(

    
            
            v is List && v.every((v) => v == null ? true :
(

    v is Map<String, dynamic>
))
),
    unDefined: () => !_reflection.arrayAndItemsNullableProp.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.arrayItemsNullable.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is List && v.every((v) => v == null ? true :
(

    v is Map<String, dynamic>
))
),
    unDefined: () => !_reflection.arrayItemsNullable.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.objectNullableProp.oasName).split<bool>(
    defined: (v) => v == null ? true :
(

    v is Map<String, dynamic>
),
    unDefined: () => !_reflection.objectNullableProp.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.objectAndItemsNullableProp.oasName).split<bool>(
    defined: (v) => v == null ? true :
(

    v is Map<String, dynamic>
),
    unDefined: () => !_reflection.objectAndItemsNullableProp.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.objectItemsNullable.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    v is Map<String, dynamic>
),
    unDefined: () => !_reflection.objectItemsNullable.required,
)) {
    return false;
  }
  if (!src.except(_reflection.knownKeys).values.every((v) => v == null ? true :
(

    v is Map<String, dynamic>
))) {
    return false;
  }
  
  return true;
}

/// Deserializes a primitive Object (num, String, List, Map).
NullableClass _$NullableClassDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$NullableClassFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$NullableClassCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$NullableClassCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Map<String,dynamic> _$NullableClassSerialize(NullableClass src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$NullableClassToXml(NullableClass instance) {
  final reflection = NullableClassXmlReflection.instance;
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

NullableClass _$NullableClassFromXml(XmlElement src) {
  final reflection = NullableClassXmlReflection.instance;
  return NullableClass.$all(

  );
}
*/

