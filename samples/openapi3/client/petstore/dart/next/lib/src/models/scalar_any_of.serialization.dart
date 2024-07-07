// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'scalar_any_of.dart';


//class serialization

Map<String, dynamic> _$ScalarAnyOfToMap(ScalarAnyOf instance) {
  final _reflection = ScalarAnyOfReflection.instance;
  return <String, dynamic>{
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

ScalarAnyOf _$ScalarAnyOfFromMap(Map<String, dynamic> src) {
  const _reflection = ScalarAnyOfReflection.instance;
  return ScalarAnyOf.$all(
        additionalProperties: AdditionalProperties(src.except(_reflection.knownKeys).map((key, v) => MapEntry(key, 
(
v

)
))),
    
    anyOf0:  UndefinedWrapper.undefined(),
    anyOf1:  UndefinedWrapper.undefined(),
    anyOf2:  UndefinedWrapper.undefined(),
  );
}

bool _$ScalarAnyOfCanFromMap(Map<String, dynamic> src) {
  final _reflection = ScalarAnyOfReflection.instance;

    if (!src.except(_reflection.knownKeys).values.every((v) => v == null ? true :
(
true
))) {
    return false;
  }



  final anyOfs = [
  
  
  ];
  final validAnyOfs = anyOfs.where((x) => x()).take(1).length;
  if (validAnyOfs == 0) {
    return false;
  }
  
  return true;
}

/// Deserializes a primitive Object (num, String, List, Map).
ScalarAnyOf _$ScalarAnyOfDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$ScalarAnyOfFromMap(src);
  } else {
    
    final v = src;
    return ScalarAnyOf.$all(

      anyOf0: (v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
)) ? UndefinedWrapper(
(

            
                    ( v is String ? v as String :




throwArgumentMismatch(String, v)

)

)


) : UndefinedWrapper.undefined(),
      anyOf1: (v == null ? false :
(

    
            
            (v is num
     || (num.tryParse(v.toString()) != null)
    
    
    
)
)) ? UndefinedWrapper(
(

            
                    ( v is num ? v as num :
num.parse(v.toString())



)

)


) : UndefinedWrapper.undefined(),
      anyOf2: (v == null ? false :
(

    
            
            (v is bool
    
     || (bool.tryParse(v.toString()) != null)
    
    
)
)) ? UndefinedWrapper(
(

            
                    ( v is bool ? v as bool :

bool.parse(v.toString())


)

)


) : UndefinedWrapper.undefined(),
      // Additional Properties only make sense if the src is a Map<String, dynamic>
      additionalProperties: AdditionalProperties(),
    );
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$ScalarAnyOfCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$ScalarAnyOfCanFromMap(src);
  } else {
    final v = src;
    final anyOfs = [
      () => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
),
      () => v == null ? false :
(

    
            
            (v is num
     || (num.tryParse(v.toString()) != null)
    
    
    
)
),
      () => v == null ? false :
(

    
            
            (v is bool
    
     || (bool.tryParse(v.toString()) != null)
    
    
)
),
    ];
    final validAnyOfs = anyOfs.where((x) => x()).take(1).length;
    if (validAnyOfs > 0) {
      return true;
    }
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Object? _$ScalarAnyOfSerialize(ScalarAnyOf src) {
  Object? initialResult = () {
    
    
    if (src.anyOf0.isDefined) {final v = src.anyOf0.valueRequired; return v; }
    
    if (src.anyOf1.isDefined) {final v = src.anyOf1.valueRequired; return v; }
    
    if (src.anyOf2.isDefined) {final v = src.anyOf2.valueRequired; return v; }
    
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
XmlElement _$ScalarAnyOfToXml(ScalarAnyOf instance) {
  final reflection = ScalarAnyOfXmlReflection.instance;
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

ScalarAnyOf _$ScalarAnyOfFromXml(XmlElement src) {
  final reflection = ScalarAnyOfXmlReflection.instance;
  return ScalarAnyOf.$all(

  );
}
*/

