// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'upload_file_with_required_file_request.reflection.dart';
part 'upload_file_with_required_file_request.serialization.dart';


/// UploadFileWithRequiredFileRequestMixin
///
/// Properties:
/// * [additionalMetadata] - Additional data to pass to server
/// * [requiredFile] - file to upload
mixin UploadFileWithRequiredFileRequestMixin on 
  
  $OpenApiObjectMixin
 {
  UndefinedWrapper<String> get additionalMetadata;
  MyFile get requiredFile;

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
  UndefinedWrapper<String> additionalMetadata;
  @override
  MyFile requiredFile;





  UploadFileWithRequiredFileRequest.$all({
    required this.additionalMetadata,
    required this.requiredFile,
    
    
  });

  UploadFileWithRequiredFileRequest({
    this.additionalMetadata = const UndefinedWrapper.undefined(),
  required  this.requiredFile ,
    
    
  });
}




