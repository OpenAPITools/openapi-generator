// Model def

import 'package:petstore_api/_internal.dart';


part 'upload_file_request.reflection.dart';
part 'upload_file_request.serialization.dart';


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
    this.additionalProperties = const AdditionalProperties(),
    
  });

  static const $reflection = UploadFileRequestReflection.instance;
  UploadFileRequestReflection get $classReflection => $reflection;

  @override
  bool validate() {
    return super.validate();
  }

  Map<String, dynamic> toMap() {
    return _$UploadFileRequestToMap(this);
  }
  factory UploadFileRequest.fromMap(Map<String, dynamic> src) {
    return _$UploadFileRequestFromMap(src);
  }
  static UploadFileRequest? fromMapOrNull(Map<String, dynamic>? src) {
    if (src == null) {
      return null;
    }
    return UploadFileRequest.fromMap(src);
  }
  static bool canFromMap(Map<String, dynamic>? src) {
    if (src  == null) {
      return false;
    }
    return _$UploadFileRequestCanFromMap(src);
  }


  /// Deserializes a primitive Object (num, String, List, Map).
  factory UploadFileRequest.deserialize(Object? src) {
    return _$UploadFileRequestDeserialize(src);
  }
  static UploadFileRequest? deserializeOrNull(Object? src) {
    if (src == null) {
      return null;
    }
    return UploadFileRequest.deserialize(src);
  }
  /// Checks if a primitive Object (num, String, List, Map) can be deserialized.
  static bool canDeserialize(Object? src) {
    return _$UploadFileRequestCanDeserialize(src);
  }
  /// Serializes to a primitive Object (num, String, List, Map).
  Map<String,dynamic> serialize() {
    return _$UploadFileRequestSerialize(this);
  }
}




