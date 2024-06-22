// ignore_for_file: unnecessary_cast, unused_local_variable

part of 'upload_file_with_required_file_request.dart';


//class serialization

Map<String, dynamic> _$UploadFileWithRequiredFileRequestToMap(UploadFileWithRequiredFileRequest instance) {
  final _reflection = UploadFileWithRequiredFileRequestReflection.instance;
  return <String, dynamic>{
    if (instance.additionalMetadata.isDefined)
    _reflection.additionalMetadata.oasName: (
            String
 v) {
      return v;
    }(instance.additionalMetadata.valueRequired),
    
    _reflection.requiredFile.oasName: (
            XFile
 v) {
      return v;
    }(instance.requiredFile),
    
    
  };
}

UploadFileWithRequiredFileRequest _$UploadFileWithRequiredFileRequestFromMap(Map<String, dynamic> src) {
  final _reflection = UploadFileWithRequiredFileRequestReflection.instance;
  return UploadFileWithRequiredFileRequest.$all(
    additionalMetadata: src.getOrUndefinedMapped(_reflection.additionalMetadata.oasName, (v) => 
(

    
            
                    v as String
            

)


),
requiredFile: src.getRequiredMapped(_reflection.requiredFile.oasName, (v) => 
(

    
            
                    v as XFile
            

)


),
    
    
  );
}

bool _$UploadFileWithRequiredFileRequestCanFromMap(Map<String, dynamic> src) {
  final _reflection = UploadFileWithRequiredFileRequestReflection.instance;
  if (!src.getOrUndefined(_reflection.additionalMetadata.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is String
),
    unDefined: () => !_reflection.additionalMetadata.required,
)) {
    return false;
  }
if (!src.getOrUndefined(_reflection.requiredFile.oasName).split<bool>(
    defined: (v) => v == null ? false :
(

    
            
            v is XFile
),
    unDefined: () => !_reflection.requiredFile.required,
)) {
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
Object? _$UploadFileWithRequiredFileRequestSerialize(UploadFileWithRequiredFileRequest src) {
  
  return src.toMap();
  
  
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

