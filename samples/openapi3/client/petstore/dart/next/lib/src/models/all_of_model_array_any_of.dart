// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'all_of_model_array_any_of.reflection.dart';
part 'all_of_model_array_any_of.serialization.dart';


//class defination

///
mixin AllOfModelArrayAnyOfMixin on 
  CategoryMixin,
  $OpenApiObjectMixin
 {
  UndefinedWrapper<AllOfModelArrayAnyOfAllOfLinkListColumn1> get linkListColumn1;
  UndefinedWrapper<AllOfModelArrayAnyOfAllOfAttributes> get attributes;


}

///
class AllOfModelArrayAnyOf with
$OpenApiObjectMixin,

CategoryMixin,
AllOfModelArrayAnyOfMixin {
  @override
  String name;
  @override
  UndefinedWrapper<AllOfModelArrayAnyOfAllOfAttributes> attributes;
  @override
  UndefinedWrapper<int> id;
  @override
  UndefinedWrapper<AllOfModelArrayAnyOfAllOfLinkListColumn1> linkListColumn1;





  AllOfModelArrayAnyOf.$all({
    required this.name,
    required this.attributes,
    required this.id,
    required this.linkListColumn1,
    
    
  });

  AllOfModelArrayAnyOf({
    this.name = 'default-name',
    this.attributes = const UndefinedWrapper.undefined(),
    this.id = const UndefinedWrapper.undefined(),
    this.linkListColumn1 = const UndefinedWrapper.undefined(),
    
    
  });
}




