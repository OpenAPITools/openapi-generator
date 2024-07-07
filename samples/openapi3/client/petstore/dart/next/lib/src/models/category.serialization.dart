// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'category.dart';


//class serialization

Map<String, dynamic> _$CategoryToMap(Category instance) {
  final _reflection = CategoryReflection.instance;
  return <String, dynamic>{
    if (instance.id.isDefined)
    _reflection.idPart.oasName: (
            int
 v) {
      return v;
    }(instance.id.valueRequired),
    
    _reflection.namePart.oasName: (
            String
 v) {
      return v;
    }(instance.name),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

Category _$CategoryFromMap(Map<String, dynamic> src) {
  const _reflection = CategoryReflection.instance;
  return Category.$all(
    id: src.getOrUndefinedMapped(_reflection.idPart.oasName, (v) => 
(

            
                    ( v is int ? v as int :
int.parse(v.toString())



)

)


),
name: src.getRequiredMapped(_reflection.namePart.oasName, (v) => 
(

            
                    ( v is String ? v as String :




throwArgumentMismatch(String, v)

)

)


),
    additionalProperties: AdditionalProperties(src.except(_reflection.knownKeys).map((key, v) => MapEntry(key, 
(
v

)
))),
    
  );
}

bool _$CategoryCanFromMap(Map<String, dynamic> src) {
  final _reflection = CategoryReflection.instance;

  if (!src.getOrUndefined(_reflection.idPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is int
     || (int.tryParse(v.toString()) != null)
    
    
    
)
),
    unDefined: () => !_reflection.idPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.namePart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
),
    unDefined: () => !_reflection.namePart.required,
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
Category _$CategoryDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$CategoryFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$CategoryCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$CategoryCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Map<String, dynamic> _$CategorySerialize(Category src) {
  Map<String, dynamic> initialResult = () {
    
    return src.toMap();
    
    
  }();
  return initialResult;
}


/*
XmlElement _$CategoryToXml(Category instance) {
  final reflection = CategoryXmlReflection.instance;
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

Category _$CategoryFromXml(XmlElement src) {
  final reflection = CategoryXmlReflection.instance;
  return Category.$all(

  );
}
*/

