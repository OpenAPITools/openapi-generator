// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'additional_properties_class.dart';


//class serialization

Map<String, dynamic> _$AdditionalPropertiesClassToMap(AdditionalPropertiesClass instance) {
  final _reflection = AdditionalPropertiesClassReflection.instance;
  return <String, dynamic>{
    if (instance.mapProperty.isDefined)
    _reflection.mapProperty.oasName: (
    Map<String, 
        
            String
>
 v) {
      return v.map((k,v) => MapEntry(k, v));
    }(instance.mapProperty.valueRequired),
    if (instance.mapOfMapProperty.isDefined)
    _reflection.mapOfMapProperty.oasName: (
    Map<String, 
        
    Map<String, 
        
            String
>
>
 v) {
      return v.map((k,v) => MapEntry(k, v.map((k,v) => MapEntry(k, v))));
    }(instance.mapOfMapProperty.valueRequired),
    if (instance.anytype1.isDefined)
    _reflection.anytype1.oasName: (Object
? v) {
      return v;
    }(instance.anytype1.valueRequired),
    if (instance.mapWithUndeclaredPropertiesAnytype1.isDefined)
    _reflection.mapWithUndeclaredPropertiesAnytype1.oasName: (
            Map<String, Object?>
 v) {
      return v;
    }(instance.mapWithUndeclaredPropertiesAnytype1.valueRequired),
    if (instance.mapWithUndeclaredPropertiesAnytype2.isDefined)
    _reflection.mapWithUndeclaredPropertiesAnytype2.oasName: (
            Map<String, Object?>
 v) {
      return v;
    }(instance.mapWithUndeclaredPropertiesAnytype2.valueRequired),
    if (instance.mapWithUndeclaredPropertiesAnytype3.isDefined)
    _reflection.mapWithUndeclaredPropertiesAnytype3.oasName: (
    Map<String, 
        Object
?>
 v) {
      return v;
    }(instance.mapWithUndeclaredPropertiesAnytype3.valueRequired),
    if (instance.emptyMap.isDefined)
    _reflection.emptyMap.oasName: (
            Map<String, Object?>
 v) {
      return v;
    }(instance.emptyMap.valueRequired),
    if (instance.mapWithUndeclaredPropertiesString.isDefined)
    _reflection.mapWithUndeclaredPropertiesString.oasName: (
    Map<String, 
        
            String
>
 v) {
      return v.map((k,v) => MapEntry(k, v));
    }(instance.mapWithUndeclaredPropertiesString.valueRequired),
    
    
  };
}

AdditionalPropertiesClass _$AdditionalPropertiesClassFromMap(Map<String, dynamic> src) {
  final _reflection = AdditionalPropertiesClassReflection.instance;
  return AdditionalPropertiesClass.$all(
    mapProperty: src.getOrUndefinedMapped(_reflection.mapProperty.oasName, (v) => 
(

    
            v as Map<String, dynamic>
            
            

)
.map((k,v) => MapEntry(k, 
(

    
            
                    v as String
            

)


))

),
mapOfMapProperty: src.getOrUndefinedMapped(_reflection.mapOfMapProperty.oasName, (v) => 
(

    
            v as Map<String, dynamic>
            
            

)
.map((k,v) => MapEntry(k, 
(

    
            v as Map<String, dynamic>
            
            

)
.map((k,v) => MapEntry(k, 
(

    
            
                    v as String
            

)


))

))

),
anytype1: src.getOrUndefinedMapped(_reflection.anytype1.oasName, (v) => 
(
v

)
),
mapWithUndeclaredPropertiesAnytype1: src.getOrUndefinedMapped(_reflection.mapWithUndeclaredPropertiesAnytype1.oasName, (v) => 
(

    v as Map<String, dynamic>

)


),
mapWithUndeclaredPropertiesAnytype2: src.getOrUndefinedMapped(_reflection.mapWithUndeclaredPropertiesAnytype2.oasName, (v) => 
(

    v as Map<String, dynamic>

)


),
mapWithUndeclaredPropertiesAnytype3: src.getOrUndefinedMapped(_reflection.mapWithUndeclaredPropertiesAnytype3.oasName, (v) => 
(

    v as Map<String, dynamic>

)
.map((k,v) => MapEntry(k, 
(
v

)
))

),
emptyMap: src.getOrUndefinedMapped(_reflection.emptyMap.oasName, (v) => 
(

    v as Map<String, dynamic>

)


),
mapWithUndeclaredPropertiesString: src.getOrUndefinedMapped(_reflection.mapWithUndeclaredPropertiesString.oasName, (v) => 
(

    
            v as Map<String, dynamic>
            
            

)
.map((k,v) => MapEntry(k, 
(

    
            
                    v as String
            

)


))

),
    
    
  );
}

bool _$AdditionalPropertiesClassCanFromMap(Map<String, dynamic> src) {
  final _reflection = AdditionalPropertiesClassReflection.instance;
  if (!src.getOrUndefined(_reflection.mapProperty.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            v is Map<String, dynamic> && v.values.every((v) => v == null ? false :
(

    
            
            v is String
))
            
),
    unDefined: () => !_reflection.mapProperty.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.mapOfMapProperty.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            v is Map<String, dynamic> && v.values.every((v) => v == null ? false :
(

    
            v is Map<String, dynamic> && v.values.every((v) => v == null ? false :
(

    
            
            v is String
))
            
))
            
),
    unDefined: () => !_reflection.mapOfMapProperty.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.anytype1.oasName).split<bool>(
    defined: (v) => v == null ? true :
(
true
),
    unDefined: () => !_reflection.anytype1.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.mapWithUndeclaredPropertiesAnytype1.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    v is Map<String, dynamic>
),
    unDefined: () => !_reflection.mapWithUndeclaredPropertiesAnytype1.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.mapWithUndeclaredPropertiesAnytype2.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    v is Map<String, dynamic>
),
    unDefined: () => !_reflection.mapWithUndeclaredPropertiesAnytype2.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.mapWithUndeclaredPropertiesAnytype3.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    v is Map<String, dynamic>
),
    unDefined: () => !_reflection.mapWithUndeclaredPropertiesAnytype3.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.emptyMap.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    v is Map<String, dynamic>
),
    unDefined: () => !_reflection.emptyMap.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.mapWithUndeclaredPropertiesString.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            v is Map<String, dynamic> && v.values.every((v) => v == null ? false :
(

    
            
            v is String
))
            
),
    unDefined: () => !_reflection.mapWithUndeclaredPropertiesString.required,
)) {
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
Object? _$AdditionalPropertiesClassSerialize(AdditionalPropertiesClass src) {
  
  return src.toMap();
  
  
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

