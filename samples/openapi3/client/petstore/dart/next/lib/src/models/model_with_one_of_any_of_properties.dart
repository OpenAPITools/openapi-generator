// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'model_with_one_of_any_of_properties.reflection.dart';
part 'model_with_one_of_any_of_properties.serialization.dart';


//class defination

///
mixin ModelWithOneOfAnyOfPropertiesMixin on 
  
  $OpenApiObjectMixin
 {
  UndefinedWrapper<ArrayOneOf> get oneofProp;
  UndefinedWrapper<ArrayAnyOf> get anyofProp;


}

///
class ModelWithOneOfAnyOfProperties with
$OpenApiObjectMixin,


ModelWithOneOfAnyOfPropertiesMixin {
  @override
  UndefinedWrapper<ArrayOneOf> oneofProp;
  @override
  UndefinedWrapper<ArrayAnyOf> anyofProp;





  ModelWithOneOfAnyOfProperties.$all({
    required this.oneofProp,
    required this.anyofProp,
    
    
  });

  ModelWithOneOfAnyOfProperties({
    this.oneofProp = const UndefinedWrapper.undefined(),
    this.anyofProp = const UndefinedWrapper.undefined(),
    
    
  });
}




