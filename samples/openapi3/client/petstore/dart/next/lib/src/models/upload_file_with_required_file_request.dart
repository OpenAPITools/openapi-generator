// Model def

import 'package:petstore_api/_internal.dart';


part 'upload_file_with_required_file_request.reflection.dart';


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

  AdditionalProperties<Object
?> additionalProperties;

  

  UploadFileWithRequiredFileRequest.$all({
        required this.additionalMetadata,
    required this.requiredFile,
    required this.additionalProperties,
    
  });

  UploadFileWithRequiredFileRequest({
      this.additionalMetadata = const UndefinedWrapper
        .undefined()
,
required  this.requiredFile     ,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = UploadFileWithRequiredFileRequestReflection.instance;
  UploadFileWithRequiredFileRequestReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory UploadFileWithRequiredFileRequest.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  UploadFileWithRequiredFileRequest clone() {
    return $reflection.clone(this);
  }
}








