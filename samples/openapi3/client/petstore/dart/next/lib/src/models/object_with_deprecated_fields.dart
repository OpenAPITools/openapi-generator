// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'object_with_deprecated_fields.reflection.dart';
part 'object_with_deprecated_fields.serialization.dart';


//class defination

///
mixin ObjectWithDeprecatedFieldsMixin on 
  
  $OpenApiObjectMixin
 {
  UndefinedWrapper<String> get uuid;
  UndefinedWrapper<num> get id;
  UndefinedWrapper<DeprecatedObject> get deprecatedRef;
  UndefinedWrapper<List<String>> get bars;


}

///
class ObjectWithDeprecatedFields with
$OpenApiObjectMixin,


ObjectWithDeprecatedFieldsMixin {
  @override
  UndefinedWrapper<String> uuid;
  @override
  UndefinedWrapper<num> id;
  @override
  UndefinedWrapper<DeprecatedObject> deprecatedRef;
  @override
  UndefinedWrapper<List<String>> bars;





  ObjectWithDeprecatedFields.$all({
    required this.uuid,
    required this.id,
    required this.deprecatedRef,
    required this.bars,
    
    
  });

  ObjectWithDeprecatedFields({
    this.uuid = const UndefinedWrapper.undefined(),
    this.id = const UndefinedWrapper.undefined(),
    this.deprecatedRef = const UndefinedWrapper.undefined(),
    this.bars = const UndefinedWrapper.undefined(),
    
    
  });
}




