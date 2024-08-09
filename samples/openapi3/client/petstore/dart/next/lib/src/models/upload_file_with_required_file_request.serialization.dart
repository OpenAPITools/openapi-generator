// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'upload_file_with_required_file_request.dart';


//class serialization

Map<String, dynamic> _$UploadFileWithRequiredFileRequestToMap(UploadFileWithRequiredFileRequest instance) {
  final _reflection = UploadFileWithRequiredFileRequestReflection.instance;
  return <String, dynamic>{
    if (instance.additionalMetadata.isDefined)
    _reflection.additionalMetadataPart.oasName: (
            String

 v) {
      return v;
    }(instance.additionalMetadata.valueRequired),
    
    _reflection.requiredFilePart.oasName: (
            XFile

 v) {
      return v;
    }(instance.requiredFile),
    ...instance.additionalProperties.map((key, v) => MapEntry(key, v)),
    
  };
}

UploadFileWithRequiredFileRequest _$UploadFileWithRequiredFileRequestFromMap(Map<String, dynamic> src) {
  const _reflection = UploadFileWithRequiredFileRequestReflection.instance;
  return UploadFileWithRequiredFileRequest.$all(
    additionalMetadata: src.getOrUndefinedMapped(_reflection.additionalMetadataPart.oasName, (v) => 
(

            
                    ( v is String ? v as String :




throwArgumentMismatch(String, v)

)

)


),
requiredFile: src.getRequiredMapped(_reflection.requiredFilePart.oasName, (v) => 
(

            
                    ( v is XFile ? v as XFile :




throwArgumentMismatch(XFile, v)

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

bool _$UploadFileWithRequiredFileRequestCanFromMap(Map<String, dynamic> src) {
  final _reflection = UploadFileWithRequiredFileRequestReflection.instance;

  if (!src.getOrUndefined(_reflection.additionalMetadataPart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is String
    
    
    
    
)
),
    unDefined: () => !_reflection.additionalMetadataPart.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.requiredFilePart.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            (v is XFile
    
    
    
    
)
),
    unDefined: () => !_reflection.requiredFilePart.required,
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
UploadFileWithRequiredFileRequest _$UploadFileWithRequiredFileRequestDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$UploadFileWithRequiredFileRequestFromMap(src);
  } else {
        
    throw UnimplementedError();
    
  }
}
/// Checks if a primitive Object (num, String, List, Map) can be deserialized.
bool _$UploadFileWithRequiredFileRequestCanDeserialize(Object? src) {
  if (src is Map<String, dynamic>) {
    return _$UploadFileWithRequiredFileRequestCanFromMap(src);
  } else {
    return false;
  }
}

/// Serializes to a primitive Object (num, String, List, Map).
Map<String, dynamic> _$UploadFileWithRequiredFileRequestSerialize(UploadFileWithRequiredFileRequest src) {
  Map<String, dynamic> initialResult = () {
    
      return _$UploadFileWithRequiredFileRequestToMap(src);
    
  }();
  return initialResult;
}


/*
XmlElement _$UploadFileWithRequiredFileRequestToXml(UploadFileWithRequiredFileRequest instance) {
  final reflection = UploadFileWithRequiredFileRequestXmlReflection.instance;
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

UploadFileWithRequiredFileRequest _$UploadFileWithRequiredFileRequestFromXml(XmlElement src) {
  final reflection = UploadFileWithRequiredFileRequestXmlReflection.instance;
  return UploadFileWithRequiredFileRequest.$all(

  );
}
*/

