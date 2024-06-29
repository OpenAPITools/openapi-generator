// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'upload_file_request.dart';


//class serialization

Map<String, dynamic> _$UploadFileRequestToMap(UploadFileRequest instance) {
  final _reflection = UploadFileRequestReflection.instance;
  return <String, dynamic>{
    if (instance.additionalMetadata.isDefined)
    _reflection.additionalMetadata.oasName: (
            String
 v) {
      return v;
    }(instance.additionalMetadata.valueRequired),
    if (instance.file.isDefined)
    _reflection.file.oasName: (
            XFile
 v) {
      return v;
    }(instance.file.valueRequired),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

UploadFileRequest _$UploadFileRequestFromMap(Map<String, dynamic> src) {
  final _reflection = UploadFileRequestReflection.instance;
  return UploadFileRequest.$all(
    additionalMetadata: src.getOrUndefinedMapped(_reflection.additionalMetadata.oasName, (v) => 
(

    
            
                    v as String
            

)


),
file: src.getOrUndefinedMapped(_reflection.file.oasName, (v) => 
(

    
            
                    v as XFile
            

)


),
    additionalProperties: AdditionalProperties(src.except(_reflection.knownKeys).map((key, v) => MapEntry(key, 
(
v

)
))),
    
  );
}

bool _$UploadFileRequestCanFromMap(Map<String, dynamic> src) {
  final _reflection = UploadFileRequestReflection.instance;
  if (!src.getOrUndefined(_reflection.additionalMetadata.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.additionalMetadata.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.file.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is XFile
),
    unDefined: () => !_reflection.file.required,
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
UploadFileRequest _$UploadFileRequestDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$UploadFileRequestFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$UploadFileRequestCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$UploadFileRequestCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Map<String,dynamic> _$UploadFileRequestSerialize(UploadFileRequest src) {
  
  return src.toMap();
  
  
}


/*
XmlElement _$UploadFileRequestToXml(UploadFileRequest instance) {
  final reflection = UploadFileRequestXmlReflection.instance;
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

UploadFileRequest _$UploadFileRequestFromXml(XmlElement src) {
  final reflection = UploadFileRequestXmlReflection.instance;
  return UploadFileRequest.$all(

  );
}
*/

