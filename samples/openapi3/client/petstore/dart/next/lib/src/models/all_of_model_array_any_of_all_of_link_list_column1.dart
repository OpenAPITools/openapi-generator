// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'all_of_model_array_any_of_all_of_link_list_column1.reflection.dart';
part 'all_of_model_array_any_of_all_of_link_list_column1.serialization.dart';


/// AllOfModelArrayAnyOfAllOfLinkListColumn1Mixin
///
/// Properties:
/// * [value] 
mixin AllOfModelArrayAnyOfAllOfLinkListColumn1Mixin on 
  
  $OpenApiObjectMixin
 {
  List<AllOfModelArrayAnyOfAllOfLinkListColumn1Value> get value;

}

/// AllOfModelArrayAnyOfAllOfLinkListColumn1
///
/// Properties:
/// * [value] 
class AllOfModelArrayAnyOfAllOfLinkListColumn1 with
$OpenApiObjectMixin,


AllOfModelArrayAnyOfAllOfLinkListColumn1Mixin {
  @override
  List<AllOfModelArrayAnyOfAllOfLinkListColumn1Value> value;





  AllOfModelArrayAnyOfAllOfLinkListColumn1.$all({
    required this.value,
    
    
  });

  AllOfModelArrayAnyOfAllOfLinkListColumn1({
  required  this.value ,
    
    
  });
}




