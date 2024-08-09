// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'array_one_of.dart';


//class serialization

Map<String, dynamic> _$ArrayOneOfToMap(ArrayOneOf instance) {
  final _reflection = ArrayOneOfReflection.instance;
  return <String, dynamic>{
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

ArrayOneOf _$ArrayOneOfFromMap(Map<String, dynamic> src) {
  const _reflection = ArrayOneOfReflection.instance;
  return ArrayOneOf.$all(
        additionalProperties: AdditionalProperties(src.except(_reflection.knownKeys).map((key, v) => MapEntry(key, 
(
v

)
))),
    
    oneOf0:  UndefinedWrapper.undefined(),
    oneOf1:  UndefinedWrapper.undefined(),
  );
}

bool _$ArrayOneOfCanFromMap(Map<String, dynamic> src) {
  final _reflection = ArrayOneOfReflection.instance;

    if (!src.except(_reflection.knownKeys).values.every((v) => v == null ? true :
(
true
))) {
    return false;
  }


  final oneOfs = [
    ];
  final validOneOfs = oneOfs.where((x) => x()).take(2).length;
  if (validOneOfs == 0 || validOneOfs > 1) {
    return false;
  }
  
  return true;
}

/// Deserializes a primitive Object (num, String, List, Map).
ArrayOneOf _$ArrayOneOfDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$ArrayOneOfFromMap(src);
  } else {
    
    final v = src;
    return ArrayOneOf.$all(
      oneOf0: (v == null ? false :
(

    
            
            (v is int
     || (int.tryParse(v.toString()) != null)
    
    
    
)
)) ? UndefinedWrapper(
(

            
                    ( v is int ? v as int :
int.parse(v.toString())



)

)


) : UndefinedWrapper.undefined(),      oneOf1: (v == null ? false :
(

    
            
            v is List && v.every((v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
))
)) ? UndefinedWrapper(
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
) : UndefinedWrapper.undefined(),
      // Additional Properties only make sense if the src is a Map<String, dynamic>
      additionalProperties: AdditionalProperties(),
    );
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$ArrayOneOfCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$ArrayOneOfCanFromMap(src);
  } else {
    final v = src;
    final oneOfs = [
      () => v == null ? false :
(

    
            
            (v is int
     || (int.tryParse(v.toString()) != null)
    
    
    
)
),
      () => v == null ? false :
(

    
            
            v is List && v.every((v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
))
),
    ];
    final validOneOfs = oneOfs.where((x) => x()).take(2).length;
    if (validOneOfs == 1) {
      return true;
    }
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Object? _$ArrayOneOfSerialize(ArrayOneOf src) {
  Object? initialResult = () {
    
      
        if (src.oneOf0.isDefined) {final v = src.oneOf0.valueRequired; return v; }
        if (src.oneOf1.isDefined) {final v = src.oneOf1.valueRequired; return v.map((v) => v).toList(); }
        return null;
  }();
  if (initialResult is Map<String, Object?>) {
    return {
      ...src.additionalProperties,
      ...initialResult,
    };
  }
  return initialResult;
}


/*
XmlElement _$ArrayOneOfToXml(ArrayOneOf instance) {
  final reflection = ArrayOneOfXmlReflection.instance;
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

ArrayOneOf _$ArrayOneOfFromXml(XmlElement src) {
  final reflection = ArrayOneOfXmlReflection.instance;
  return ArrayOneOf.$all(

  );
}
*/

