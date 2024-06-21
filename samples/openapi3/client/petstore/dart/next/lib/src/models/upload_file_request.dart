// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'upload_file_request.reflection.dart';
part 'upload_file_request.serialization.dart';


/// UploadFileRequestMixin
///
/// Properties:
/// * [additionalMetadata] - Additional data to pass to server
/// * [file] - file to upload
mixin UploadFileRequestMixin on 
  
  $OpenApiObjectMixin
 {
  UndefinedWrapper<String> get additionalMetadata;
  UndefinedWrapper<MyFile> get file;

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
  UndefinedWrapper<String> additionalMetadata;
  @override
  UndefinedWrapper<MyFile> file;





  UploadFileRequest.$all({
    required this.additionalMetadata,
    required this.file,
    
    
  });

  UploadFileRequest({
    this.additionalMetadata = const UndefinedWrapper.undefined(),
    this.file = const UndefinedWrapper.undefined(),
    
    
  });
}




