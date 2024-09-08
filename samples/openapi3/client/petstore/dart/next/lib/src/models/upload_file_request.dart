// Model def

import 'package:petstore_api/_internal.dart';


part 'upload_file_request.reflection.dart';


/// UploadFileRequestMixin
///
/// Properties:
/// * [additionalMetadata] - Additional data to pass to server
/// * [file] - file to upload
mixin UploadFileRequestMixin on
  $OpenApiObjectMixin {
  UndefinedWrapper<
            String
> get additionalMetadata;
UndefinedWrapper<
            XFile
> get file;
  
}

/// UploadFileRequest
///
/// Properties:
/// * [additionalMetadata] - Additional data to pass to server
/// * [file] - file to upload
class UploadFileRequest with
$OpenApiObjectMixin,

UploadFileRequestMixin {
  @override
  UndefinedWrapper<
            String
> additionalMetadata;
  @override
  UndefinedWrapper<
            XFile
> file;

  AdditionalProperties<Object
?> additionalProperties;

  

  UploadFileRequest.$all({
        required this.additionalMetadata,
    required this.file,
    required this.additionalProperties,
    
  });

  UploadFileRequest({
      this.additionalMetadata = const UndefinedWrapper
        .undefined()
,
  this.file = const UndefinedWrapper
        .undefined()
,
    AdditionalProperties<Object
?>? additionalProperties,
    
  }) : additionalProperties = additionalProperties ?? {};

  static const $reflection = UploadFileRequestReflection.instance;
  UploadFileRequestReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  factory UploadFileRequest.deserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.deserialize(src, context);
  }

  static bool canDeserialize(Object? src, [SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.canDeserialize(src, context);
  }

  Object? serialize([SerializationContext context = const SerializationContext.json(),]) {
    return $reflection.serialize(this, context);
  }

  UploadFileRequest clone() {
    return $reflection.clone(this);
  }
}








