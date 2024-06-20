// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'value.reflection.dart';
part 'value.serialization.dart';


//class defination

///
mixin ValueMixin on 
  
  $OpenApiObjectMixin
 {


UndefinedWrapper<Scalar> get oneOf0;

UndefinedWrapper<List<Scalar>> get oneOf1;

}

///
class Value with
$OpenApiObjectMixin,


ValueMixin {




  @override
UndefinedWrapper<Scalar> oneOf0;

  @override
UndefinedWrapper<List<Scalar>> oneOf1;


  Value.$all({
    
    
    required this.oneOf0,
    
    required this.oneOf1,
    
  });

  Value({
    
    
    this.oneOf0 = const UndefinedWrapper.undefined(),
    
    this.oneOf1 = const UndefinedWrapper.undefined(),
    
  });
}




