// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'array_default.reflection.dart';
part 'array_default.serialization.dart';


/// ArrayDefaultMixin
///
/// Properties:
/// * [withDefaultEmptyBracket] 
/// * [withoutDefault] 
mixin ArrayDefaultMixin on 
  
  $OpenApiObjectMixin
 {
  UndefinedWrapper<List<String>> get withDefaultEmptyBracket;
  UndefinedWrapper<List<String>> get withoutDefault;

}

/// ArrayDefault
///
/// Properties:
/// * [withDefaultEmptyBracket] 
/// * [withoutDefault] 
class ArrayDefault with
$OpenApiObjectMixin,


ArrayDefaultMixin {
  @override
  UndefinedWrapper<List<String>> withDefaultEmptyBracket;
  @override
  UndefinedWrapper<List<String>> withoutDefault;





  ArrayDefault.$all({
    required this.withDefaultEmptyBracket,
    required this.withoutDefault,
    
    
  });

  ArrayDefault({
    this.withDefaultEmptyBracket = const UndefinedWrapper([]),
    this.withoutDefault = const UndefinedWrapper.undefined(),
    
    
  });
}




