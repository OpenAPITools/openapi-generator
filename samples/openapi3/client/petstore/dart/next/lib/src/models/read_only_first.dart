// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'read_only_first.reflection.dart';
part 'read_only_first.serialization.dart';


//class defination

///
mixin ReadOnlyFirstMixin on 
  
  $OpenApiObjectMixin
 {
  UndefinedWrapper<String> get bar;
  UndefinedWrapper<String> get baz;


}

///
class ReadOnlyFirst with
$OpenApiObjectMixin,


ReadOnlyFirstMixin {
  @override
  UndefinedWrapper<String> bar;
  @override
  UndefinedWrapper<String> baz;





  ReadOnlyFirst.$all({
    required this.bar,
    required this.baz,
    
    
  });

  ReadOnlyFirst({
    this.bar = const UndefinedWrapper.undefined(),
    this.baz = const UndefinedWrapper.undefined(),
    
    
  });
}




