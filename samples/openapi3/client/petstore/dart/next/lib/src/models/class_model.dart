// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'class_model.reflection.dart';
part 'class_model.serialization.dart';


//class defination

///
mixin ClassModelMixin on 
  
  $OpenApiObjectMixin
 {
  UndefinedWrapper<String> get propertyClass;


}

///
class ClassModel with
$OpenApiObjectMixin,


ClassModelMixin {
  @override
  UndefinedWrapper<String> propertyClass;





  ClassModel.$all({
    required this.propertyClass,
    
    
  });

  ClassModel({
    this.propertyClass = const UndefinedWrapper.undefined(),
    
    
  });
}




