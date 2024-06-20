// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'file.reflection.dart';
part 'file.serialization.dart';


/// Must be named `File` for test.
///
/// Properties:
/// * [sourceURI] - Test capitalization
mixin FileMixin on 
  
  $OpenApiObjectMixin
 {
  UndefinedWrapper<String> get sourceURI;

}

/// Must be named `File` for test.
///
/// Properties:
/// * [sourceURI] - Test capitalization
class File with
$OpenApiObjectMixin,


FileMixin {
  @override
  UndefinedWrapper<String> sourceURI;





  File.$all({
    required this.sourceURI,
    
    
  });

  File({
    this.sourceURI = const UndefinedWrapper.undefined(),
    
    
  });
}




