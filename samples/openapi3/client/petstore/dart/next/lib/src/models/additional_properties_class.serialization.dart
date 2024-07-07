// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'additional_properties_class.dart';


//class serialization

Map<String, dynamic> _$AdditionalPropertiesClassToMap(AdditionalPropertiesClass instance) {
  final _reflection = AdditionalPropertiesClassReflection.instance;
  return <String, dynamic>{
    if (instance.mapProperty.isDefined)
    _reflection.mapPropertyPart.oasName: (
    Map<String, 
        
            String
>
 v) {
      return v.map((k,v) => MapEntry(k, v));
    }(instance.mapProperty.valueRequired),
    if (instance.mapOfMapProperty.isDefined)
    _reflection.mapOfMapPropertyPart.oasName: (
    Map<String, 
        
    Map<String, 
        
            String
>
>
 v) {
      return v.map((k,v) => MapEntry(k, v.map((k,v) => MapEntry(k, v))));
    }(instance.mapOfMapProperty.valueRequired),
    if (instance.anytype1.isDefined)
    _reflection.anytype1Part.oasName: (Object
? v) {
      return v;
    }(instance.anytype1.valueRequired),
    if (instance.mapWithUndeclaredPropertiesAnytype1.isDefined)
    _reflection.mapWithUndeclaredPropertiesAnytype1Part.oasName: (
            $FreeFormObject
 v) {
      return v;
    }(instance.mapWithUndeclaredPropertiesAnytype1.valueRequired),
    if (instance.mapWithUndeclaredPropertiesAnytype2.isDefined)
    _reflection.mapWithUndeclaredPropertiesAnytype2Part.oasName: (
            $FreeFormObject
 v) {
      return v;
    }(instance.mapWithUndeclaredPropertiesAnytype2.valueRequired),
    if (instance.mapWithUndeclaredPropertiesAnytype3.isDefined)
    _reflection.mapWithUndeclaredPropertiesAnytype3Part.oasName: (
    Map<String, 
        Object
?>
 v) {
      return v;
    }(instance.mapWithUndeclaredPropertiesAnytype3.valueRequired),
    if (instance.emptyMap.isDefined)
    _reflection.emptyMapPart.oasName: (
            $FreeFormObject
 v) {
      return v;
    }(instance.emptyMap.valueRequired),
    if (instance.mapWithUndeclaredPropertiesString.isDefined)
    _reflection.mapWithUndeclaredPropertiesStringPart.oasName: (
    Map<String, 
        
            String
>
 v) {
      return v.map((k,v) => MapEntry(k, v));
    }(instance.mapWithUndeclaredPropertiesString.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

AdditionalPropertiesClass _$AdditionalPropertiesClassFromMap(Map<String, dynamic> src) {
  const _reflection = AdditionalPropertiesClassReflection.instance;
  return AdditionalPropertiesClass.$all(
    mapProperty: src.getOrUndefinedMapped(_reflection.mapPropertyPart.oasName, (v) => 
(

            v as Map<String, dynamic>
            
            

)
.map((k,v) => MapEntry(k, 
(

            
                    ( v is String ? v as String :




throwArgumentMismatch(String, v)

)

)


))

),
mapOfMapProperty: src.getOrUndefinedMapped(_reflection.mapOfMapPropertyPart.oasName, (v) => 
(

            v as Map<String, dynamic>
            
            

)
.map((k,v) => MapEntry(k, 
(

            v as Map<String, dynamic>
            
            

)
.map((k,v) => MapEntry(k, 
(

            
                    ( v is String ? v as String :




throwArgumentMismatch(String, v)

)

)


))

))

),
anytype1: src.getOrUndefinedMapped(_reflection.anytype1Part.oasName, (v) => 
(
v

)
),
mapWithUndeclaredPropertiesAnytype1: src.getOrUndefinedMapped(_reflection.mapWithUndeclaredPropertiesAnytype1Part.oasName, (v) => 
(

            
                    ( v is $FreeFormObject ? v as $FreeFormObject :




throwArgumentMismatch($FreeFormObject, v)

)

)


),
mapWithUndeclaredPropertiesAnytype2: src.getOrUndefinedMapped(_reflection.mapWithUndeclaredPropertiesAnytype2Part.oasName, (v) => 
(

            
                    ( v is $FreeFormObject ? v as $FreeFormObject :




throwArgumentMismatch($FreeFormObject, v)

)

)


),
mapWithUndeclaredPropertiesAnytype3: src.getOrUndefinedMapped(_reflection.mapWithUndeclaredPropertiesAnytype3Part.oasName, (v) => 
(

            v as Map<String, dynamic>
            
            

)
.map((k,v) => MapEntry(k, 
(
v

)
))

),
emptyMap: src.getOrUndefinedMapped(_reflection.emptyMapPart.oasName, (v) => 
(

            
                    ( v is $FreeFormObject ? v as $FreeFormObject :




throwArgumentMismatch($FreeFormObject, v)

)

)


),
mapWithUndeclaredPropertiesString: src.getOrUndefinedMapped(_reflection.mapWithUndeclaredPropertiesStringPart.oasName, (v) => 
(

            v as Map<String, dynamic>
            
            

)
.map((k,v) => MapEntry(k, 
(

            
                    ( v is String ? v as String :




throwArgumentMismatch(String, v)

)

)


))

),
    additionalProperties: AdditionalProperties(src.except(_reflection.knownKeys).map((key, v) => MapEntry(key, 
(
v

)
))),
    
  );
}

bool _$AdditionalPropertiesClassCanFromMap(Map<String, dynamic> src) {
  final _reflection = AdditionalPropertiesClassReflection.instance;

  if (!src.getOrUndefined(_reflection.mapPropertyPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            v is Map<String, dynamic> && v.values.every((v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
))
            
),
    unDefined: () => !_reflection.mapPropertyPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.mapOfMapPropertyPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            v is Map<String, dynamic> && v.values.every((v) => v == null ? false :
(

    
            v is Map<String, dynamic> && v.values.every((v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
))
            
))
            
),
    unDefined: () => !_reflection.mapOfMapPropertyPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.anytype1Part.oasName).split<bool>(
    defined: (v) => v == null ? true :
(
true
),
    unDefined: () => !_reflection.anytype1Part.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.mapWithUndeclaredPropertiesAnytype1Part.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    v is Map<String, dynamic>
),
    unDefined: () => !_reflection.mapWithUndeclaredPropertiesAnytype1Part.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.mapWithUndeclaredPropertiesAnytype2Part.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    v is Map<String, dynamic>
),
    unDefined: () => !_reflection.mapWithUndeclaredPropertiesAnytype2Part.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.mapWithUndeclaredPropertiesAnytype3Part.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    v is Map<String, dynamic>
),
    unDefined: () => !_reflection.mapWithUndeclaredPropertiesAnytype3Part.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.emptyMapPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    v is Map<String, dynamic>
),
    unDefined: () => !_reflection.emptyMapPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.mapWithUndeclaredPropertiesStringPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            v is Map<String, dynamic> && v.values.every((v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
))
            
),
    unDefined: () => !_reflection.mapWithUndeclaredPropertiesStringPart.required,
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
AdditionalPropertiesClass _$AdditionalPropertiesClassDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$AdditionalPropertiesClassFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$AdditionalPropertiesClassCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$AdditionalPropertiesClassCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Map<String, dynamic> _$AdditionalPropertiesClassSerialize(AdditionalPropertiesClass src) {
  Map<String, dynamic> initialResult = () {
    
    return src.toMap();
    
    
  }();
  return initialResult;
}


/*
XmlElement _$AdditionalPropertiesClassToXml(AdditionalPropertiesClass instance) {
  final reflection = AdditionalPropertiesClassXmlReflection.instance;
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

AdditionalPropertiesClass _$AdditionalPropertiesClassFromXml(XmlElement src) {
  final reflection = AdditionalPropertiesClassXmlReflection.instance;
  return AdditionalPropertiesClass.$all(

  );
}
*/

