// Model def

import 'package:openapi/_internal.dart';


part 'upload_file_with_required_file_request.reflection.dart';
part 'upload_file_with_required_file_request.serialization.dart';


/// UploadFileWithRequiredFileRequestMixin
///
/// Properties:
/// * [additionalMetadata] - Additional data to pass to server
/// * [requiredFile] - file to upload
mixin UploadFileWithRequiredFileRequestMixin on 
  $OpenApiObjectMixin {
  UndefinedWrapper<
            String
> get additionalMetadata;

            XFile
 get requiredFile;
  
}

/// UploadFileWithRequiredFileRequest
///
/// Properties:
/// * [additionalMetadata] - Additional data to pass to server
/// * [requiredFile] - file to upload
class UploadFileWithRequiredFileRequest with
$OpenApiObjectMixin,


UploadFileWithRequiredFileRequestMixin {
  @override
  UndefinedWrapper<
            String
> additionalMetadata;
  @override
  
            XFile
 requiredFile;

  

  

  UploadFileWithRequiredFileRequest.$all({
        required this.additionalMetadata,
    required this.requiredFile,
    
    
  });

  UploadFileWithRequiredFileRequest({
      this.additionalMetadata = const UndefinedWrapper
        .undefined()
,
required  this.requiredFile     ,
    
    
  });

  static const $reflection = UploadFileWithRequiredFileRequestReflection.instance;

  @override
  bool validate() {
    return super.validate();
  }


  Map<String, dynamic> toMap() {
    return _$UploadFileWithRequiredFileRequestToMap(this);
  }
  factory UploadFileWithRequiredFileRequest.fromMap(Map<String, dynamic> src) {
    return _$UploadFileWithRequiredFileRequestFromMap(src);
  }
  static UploadFileWithRequiredFileRequest? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return UploadFileWithRequiredFileRequest.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$UploadFileWithRequiredFileRequestCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory UploadFileWithRequiredFileRequest.deserialize(Object? src) {
    return _$UploadFileWithRequiredFileRequestDeserialize(src);
  }
  static UploadFileWithRequiredFileRequest? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return UploadFileWithRequiredFileRequest.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$UploadFileWithRequiredFileRequestCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Object? serialize() {
    return _$UploadFileWithRequiredFileRequestSerialize(this);
  }
}




