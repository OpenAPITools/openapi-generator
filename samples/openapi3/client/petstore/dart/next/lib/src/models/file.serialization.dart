// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'file.dart';


//class serialization

Map<String, dynamic> _$FileToMap(File instance) {
  final _reflection = FileReflection.instance;
  return <String, dynamic>{
    if (instance.sourceURI.isDefined)
    _reflection.sourceURIPart.oasName: (
            String
 v) {
      return v;
    }(instance.sourceURI.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

File _$FileFromMap(Map<String, dynamic> src) {
  const _reflection = FileReflection.instance;
  return File.$all(
    sourceURI: src.getOrUndefinedMapped(_reflection.sourceURIPart.oasName, (v) => 
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

bool _$FileCanFromMap(Map<String, dynamic> src) {
  final _reflection = FileReflection.instance;

  if (!src.getOrUndefined(_reflection.sourceURIPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
),
    unDefined: () => !_reflection.sourceURIPart.required,
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
File _$FileDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$FileFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$FileCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$FileCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Map<String, dynamic> _$FileSerialize(File src) {
  Map<String, dynamic> initialResult = () {
    
    return src.toMap();
    
    
  }();
  return initialResult;
}


/*
XmlElement _$FileToXml(File instance) {
  final reflection = FileXmlReflection.instance;
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

File _$FileFromXml(XmlElement src) {
  final reflection = FileXmlReflection.instance;
  return File.$all(

  );
}
*/

