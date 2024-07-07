// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'file_schema_test_class.dart';


//class serialization

Map<String, dynamic> _$FileSchemaTestClassToMap(FileSchemaTestClass instance) {
  final _reflection = FileSchemaTestClassReflection.instance;
  return <String, dynamic>{
    if (instance.file.isDefined)
    _reflection.filePart.oasName: (
            File
 v) {
      return v.serialize();
    }(instance.file.valueRequired),
    if (instance.files.isDefined)
    _reflection.filesPart.oasName: (
    List<
        
            File
>
 v) {
      return v.map((v) => v.serialize()).toList();
    }(instance.files.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

FileSchemaTestClass _$FileSchemaTestClassFromMap(Map<String, dynamic> src) {
  const _reflection = FileSchemaTestClassReflection.instance;
  return FileSchemaTestClass.$all(
    file: src.getOrUndefinedMapped(_reflection.filePart.oasName, (v) => File.deserialize
(

            v

)


),
files: src.getOrUndefinedMapped(_reflection.filesPart.oasName, (v) => 
(

            
            v as List
            

)

.map((v) => File.deserialize
(

            v

)


).toList()
),
    additionalProperties: AdditionalProperties(src.except(_reflection.knownKeys).map((key, v) => MapEntry(key, 
(
v

)
))),
    
  );
}

bool _$FileSchemaTestClassCanFromMap(Map<String, dynamic> src) {
  final _reflection = FileSchemaTestClassReflection.instance;

  if (!src.getOrUndefined(_reflection.filePart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            File.canDeserialize(v)
            
),
    unDefined: () => !_reflection.filePart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.filesPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is List && v.every((v) => v == null ? false :
(

    
            File.canDeserialize(v)
            
))
),
    unDefined: () => !_reflection.filesPart.required,
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
FileSchemaTestClass _$FileSchemaTestClassDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$FileSchemaTestClassFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$FileSchemaTestClassCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$FileSchemaTestClassCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Map<String, dynamic> _$FileSchemaTestClassSerialize(FileSchemaTestClass src) {
  Map<String, dynamic> initialResult = () {
    
    return src.toMap();
    
    
  }();
  return initialResult;
}


/*
XmlElement _$FileSchemaTestClassToXml(FileSchemaTestClass instance) {
  final reflection = FileSchemaTestClassXmlReflection.instance;
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

FileSchemaTestClass _$FileSchemaTestClassFromXml(XmlElement src) {
  final reflection = FileSchemaTestClassXmlReflection.instance;
  return FileSchemaTestClass.$all(

  );
}
*/

