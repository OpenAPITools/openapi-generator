// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'tag.reflection.dart';
part 'tag.serialization.dart';


/// TagMixin
///
/// Properties:
/// * [id] 
/// * [name] 
mixin TagMixin on 
  
  $OpenApiObjectMixin
 {
  UndefinedWrapper<int> get id;
  UndefinedWrapper<String> get name;

}

/// Tag
///
/// Properties:
/// * [id] 
/// * [name] 
class Tag with
$OpenApiObjectMixin,


TagMixin {
  @override
  UndefinedWrapper<int> id;
  @override
  UndefinedWrapper<String> name;





  Tag.$all({
    required this.id,
    required this.name,
    
    
  });

  Tag({
    this.id = const UndefinedWrapper.undefined(),
    this.name = const UndefinedWrapper.undefined(),
    
    
  });
}




