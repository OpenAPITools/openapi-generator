// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'nullable_class.dart';


//class serialization

Map<String, dynamic> _$NullableClassToMap(NullableClass instance) {
  final _reflection = NullableClassReflection.instance;
  return <String, dynamic>{
    if (instance.integerProp.isDefined)
    _reflection.integerPropPart.oasName: (
            int

? v) {
      return v;
    }(instance.integerProp.valueRequired),
    if (instance.numberProp.isDefined)
    _reflection.numberPropPart.oasName: (
            num

? v) {
      return v;
    }(instance.numberProp.valueRequired),
    if (instance.booleanProp.isDefined)
    _reflection.booleanPropPart.oasName: (
            bool

? v) {
      return v;
    }(instance.booleanProp.valueRequired),
    if (instance.stringProp.isDefined)
    _reflection.stringPropPart.oasName: (
            String

? v) {
      return v;
    }(instance.stringProp.valueRequired),
    if (instance.dateProp.isDefined)
    _reflection.datePropPart.oasName: (
            DateTime

? v) {
      return v;
    }(instance.dateProp.valueRequired),
    if (instance.datetimeProp.isDefined)
    _reflection.datetimePropPart.oasName: (
            DateTime

? v) {
      return v;
    }(instance.datetimeProp.valueRequired),
    if (instance.arrayNullableProp.isDefined)
    _reflection.arrayNullablePropPart.oasName: (
    List<
        
            $FreeFormObject

>

? v) {
      return v?.map((v) => v).toList();
    }(instance.arrayNullableProp.valueRequired),
    if (instance.arrayAndItemsNullableProp.isDefined)
    _reflection.arrayAndItemsNullablePropPart.oasName: (
    List<
        
            $FreeFormObject

?>

? v) {
      return v?.map((v) => v).toList();
    }(instance.arrayAndItemsNullableProp.valueRequired),
    if (instance.arrayItemsNullable.isDefined)
    _reflection.arrayItemsNullablePart.oasName: (
    List<
        
            $FreeFormObject

?>

 v) {
      return v.map((v) => v).toList();
    }(instance.arrayItemsNullable.valueRequired),
    if (instance.objectNullableProp.isDefined)
    _reflection.objectNullablePropPart.oasName: (
    Map<String, 
        
            $FreeFormObject

>

? v) {
      return v;
    }(instance.objectNullableProp.valueRequired),
    if (instance.objectAndItemsNullableProp.isDefined)
    _reflection.objectAndItemsNullablePropPart.oasName: (
    Map<String, 
        
            $FreeFormObject

?>

? v) {
      return v;
    }(instance.objectAndItemsNullableProp.valueRequired),
    if (instance.objectItemsNullable.isDefined)
    _reflection.objectItemsNullablePart.oasName: (
    Map<String, 
        
            $FreeFormObject

?>

 v) {
      return v;
    }(instance.objectItemsNullable.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

NullableClass _$NullableClassFromMap(Map<String, dynamic> src) {
  const _reflection = NullableClassReflection.instance;
  return NullableClass.$all(
    integerProp: src.getOrUndefinedMapped(_reflection.integerPropPart.oasName, (v) => 
(

            
                    (v == null ? null :  v is int ? v as int :
int.parse(v.toString())



)

)


),
numberProp: src.getOrUndefinedMapped(_reflection.numberPropPart.oasName, (v) => 
(

            
                    (v == null ? null :  v is num ? v as num :
num.parse(v.toString())



)

)


),
booleanProp: src.getOrUndefinedMapped(_reflection.booleanPropPart.oasName, (v) => 
(

            
                    (v == null ? null :  v is bool ? v as bool :

bool.parse(v.toString())


)

)


),
stringProp: src.getOrUndefinedMapped(_reflection.stringPropPart.oasName, (v) => 
(

            
                    (v == null ? null :  v is String ? v as String :




throwArgumentMismatch(String, v)

)

)


),
dateProp: src.getOrUndefinedMapped(_reflection.datePropPart.oasName, (v) => 
(

            
                    (v == null ? null :  v is DateTime ? v as DateTime :




throwArgumentMismatch(DateTime, v)

)

)


),
datetimeProp: src.getOrUndefinedMapped(_reflection.datetimePropPart.oasName, (v) => 
(

            
                    (v == null ? null :  v is DateTime ? v as DateTime :


v is int ? DateTime.fromMillisecondsSinceEpoch(v as int) : DateTime.parse(v.toString())

)

)


),
arrayNullableProp: src.getOrUndefinedMapped(_reflection.arrayNullablePropPart.oasName, (v) => 
(

            
            v as List
            ?

)

?.map((v) => 
(

            
                    ( v is $FreeFormObject ? v as $FreeFormObject :




throwArgumentMismatch($FreeFormObject, v)

)

)


).toList()
),
arrayAndItemsNullableProp: src.getOrUndefinedMapped(_reflection.arrayAndItemsNullablePropPart.oasName, (v) => 
(

            
            v as List
            ?

)

?.map((v) => 
(

            
                    (v == null ? null :  v is $FreeFormObject ? v as $FreeFormObject :




throwArgumentMismatch($FreeFormObject, v)

)

)


).toList()
),
arrayItemsNullable: src.getOrUndefinedMapped(_reflection.arrayItemsNullablePart.oasName, (v) => 
(

            
            v as List
            

)

.map((v) => 
(

            
                    (v == null ? null :  v is $FreeFormObject ? v as $FreeFormObject :




throwArgumentMismatch($FreeFormObject, v)

)

)


).toList()
),
objectNullableProp: src.getOrUndefinedMapped(_reflection.objectNullablePropPart.oasName, (v) => 
(

            v as Map<String, dynamic>
            
            ?

)
?.map((k,v) => MapEntry(k, 
(

            
                    ( v is $FreeFormObject ? v as $FreeFormObject :




throwArgumentMismatch($FreeFormObject, v)

)

)


))

),
objectAndItemsNullableProp: src.getOrUndefinedMapped(_reflection.objectAndItemsNullablePropPart.oasName, (v) => 
(

            v as Map<String, dynamic>
            
            ?

)
?.map((k,v) => MapEntry(k, 
(

            
                    (v == null ? null :  v is $FreeFormObject ? v as $FreeFormObject :




throwArgumentMismatch($FreeFormObject, v)

)

)


))

),
objectItemsNullable: src.getOrUndefinedMapped(_reflection.objectItemsNullablePart.oasName, (v) => 
(

            v as Map<String, dynamic>
            
            

)
.map((k,v) => MapEntry(k, 
(

            
                    (v == null ? null :  v is $FreeFormObject ? v as $FreeFormObject :




throwArgumentMismatch($FreeFormObject, v)

)

)


))

),
    additionalProperties: AdditionalProperties(src.except(_reflection.knownKeys).map((key, v) => MapEntry(key, 
(

            
                    (v == null ? null :  v is $FreeFormObject ? v as $FreeFormObject :




throwArgumentMismatch($FreeFormObject, v)

)

)


))),
    
  );
}

bool _$NullableClassCanFromMap(Map<String, dynamic> src) {
  final _reflection = NullableClassReflection.instance;

  if (!src.getOrUndefined(_reflection.integerPropPart.oasName).split<bool>(
    defined: (v) => v == null ? true :
(

    
            
            (v is int
     || (int.tryParse(v.toString()) != null)
    
    
    
)
),
    unDefined: () => !_reflection.integerPropPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.numberPropPart.oasName).split<bool>(
    defined: (v) => v == null ? true :
(

    
            
            (v is num
     || (num.tryParse(v.toString()) != null)
    
    
    
)
),
    unDefined: () => !_reflection.numberPropPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.booleanPropPart.oasName).split<bool>(
    defined: (v) => v == null ? true :
(

    
            
            (v is bool
    
     || (bool.tryParse(v.toString()) != null)
    
    
)
),
    unDefined: () => !_reflection.booleanPropPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.stringPropPart.oasName).split<bool>(
    defined: (v) => v == null ? true :
(

    
            
            (v is String
    
    
    
    
)
),
    unDefined: () => !_reflection.stringPropPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.datePropPart.oasName).split<bool>(
    defined: (v) => v == null ? true :
(

    
            
            (v is DateTime
    
    
    
    
)
),
    unDefined: () => !_reflection.datePropPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.datetimePropPart.oasName).split<bool>(
    defined: (v) => v == null ? true :
(

    
            
            (v is DateTime
    
    
     || (v is int || DateTime.tryParse(v.toString()) != null)
    
)
),
    unDefined: () => !_reflection.datetimePropPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.arrayNullablePropPart.oasName).split<bool>(
    defined: (v) => v == null ? true :
(

    
            
            v is List && v.every((v) => v == null ? false :
(

    v is Map<String, dynamic>
))
),
    unDefined: () => !_reflection.arrayNullablePropPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.arrayAndItemsNullablePropPart.oasName).split<bool>(
    defined: (v) => v == null ? true :
(

    
            
            v is List && v.every((v) => v == null ? true :
(

    v is Map<String, dynamic>
))
),
    unDefined: () => !_reflection.arrayAndItemsNullablePropPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.arrayItemsNullablePart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is List && v.every((v) => v == null ? true :
(

    v is Map<String, dynamic>
))
),
    unDefined: () => !_reflection.arrayItemsNullablePart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.objectNullablePropPart.oasName).split<bool>(
    defined: (v) => v == null ? true :
(

    v is Map<String, dynamic>
),
    unDefined: () => !_reflection.objectNullablePropPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.objectAndItemsNullablePropPart.oasName).split<bool>(
    defined: (v) => v == null ? true :
(

    v is Map<String, dynamic>
),
    unDefined: () => !_reflection.objectAndItemsNullablePropPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.objectItemsNullablePart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    v is Map<String, dynamic>
),
    unDefined: () => !_reflection.objectItemsNullablePart.required,
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
Map<String, dynamic> _$NullableClassSerialize(NullableClass src) {
  Map<String, dynamic> initialResult = () {
    
      return _$NullableClassToMap(src);
    
  }();
  return initialResult;
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

