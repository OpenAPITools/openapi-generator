// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'mammal.reflection.dart';
part 'mammal.serialization.dart';


/// MammalMixin
///
/// Properties:
mixin MammalMixin on 
  
  $OpenApiObjectMixin
 {

  UndefinedWrapper<Whale> get oneOf0;
  UndefinedWrapper<Zebra> get oneOf1;
  UndefinedWrapper<Pig> get oneOf2;
}

/// Mammal
///
/// Properties:
class Mammal with
$OpenApiObjectMixin,


MammalMixin {




  @override
  UndefinedWrapper<Whale> oneOf0;

  @override
  UndefinedWrapper<Zebra> oneOf1;

  @override
  UndefinedWrapper<Pig> oneOf2;


  Mammal.$all({
    
    
    required this.oneOf0,
    required this.oneOf1,
    required this.oneOf2,
  });

  Mammal({
    
    
    this.oneOf0 = const UndefinedWrapper.undefined(),
    this.oneOf1 = const UndefinedWrapper.undefined(),
    this.oneOf2 = const UndefinedWrapper.undefined(),
  });
}




