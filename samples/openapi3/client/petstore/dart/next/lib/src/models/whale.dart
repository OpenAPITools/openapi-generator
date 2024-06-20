// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'whale.reflection.dart';
part 'whale.serialization.dart';


/// WhaleMixin
///
/// Properties:
/// * [hasBaleen] 
/// * [hasTeeth] 
/// * [className] 
mixin WhaleMixin on 
  
  $OpenApiObjectMixin
 {
  UndefinedWrapper<bool> get hasBaleen;
  UndefinedWrapper<bool> get hasTeeth;
  String get className;

}

/// Whale
///
/// Properties:
/// * [hasBaleen] 
/// * [hasTeeth] 
/// * [className] 
class Whale with
$OpenApiObjectMixin,


WhaleMixin {
  @override
  UndefinedWrapper<bool> hasBaleen;
  @override
  UndefinedWrapper<bool> hasTeeth;
  @override
  String className;





  Whale.$all({
    required this.hasBaleen,
    required this.hasTeeth,
    required this.className,
    
    
  });

  Whale({
    this.hasBaleen = const UndefinedWrapper.undefined(),
    this.hasTeeth = const UndefinedWrapper.undefined(),
  required  this.className ,
    
    
  });
}




