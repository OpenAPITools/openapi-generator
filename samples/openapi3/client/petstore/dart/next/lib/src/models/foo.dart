// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'foo.reflection.dart';
part 'foo.serialization.dart';


//class defination

///
mixin FooMixin on 
  
  $OpenApiObjectMixin
 {
  UndefinedWrapper<String> get bar;


}

///
class Foo with
$OpenApiObjectMixin,


FooMixin {
  @override
  UndefinedWrapper<String> bar;





  Foo.$all({
    required this.bar,
    
    
  });

  Foo({
    this.bar = const UndefinedWrapper('bar'),
    
    
  });
}




