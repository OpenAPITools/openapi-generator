// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'array_any_of.reflection.dart';
part 'array_any_of.serialization.dart';


/// ArrayAnyOfMixin
mixin ArrayAnyOfMixin on 
  
  $OpenApiObjectMixin
 {

  UndefinedWrapper<int> get anyOf0;
  UndefinedWrapper<List<String>> get anyOf1;
}

/// ArrayAnyOf
class ArrayAnyOf with
$OpenApiObjectMixin,


ArrayAnyOfMixin {




  @override
  UndefinedWrapper<int> anyOf0;

  @override
  UndefinedWrapper<List<String>> anyOf1;


  ArrayAnyOf.$all({
    
    
    required this.anyOf0,
    required this.anyOf1,
  });

  ArrayAnyOf({
    
    
    this.anyOf0 = const UndefinedWrapper.undefined(),
    
    this.anyOf1 = const UndefinedWrapper.undefined(),
    
  });
}




