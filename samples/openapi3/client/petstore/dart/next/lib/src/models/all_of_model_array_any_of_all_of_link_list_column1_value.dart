// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'all_of_model_array_any_of_all_of_link_list_column1_value.reflection.dart';
part 'all_of_model_array_any_of_all_of_link_list_column1_value.serialization.dart';


/// AllOfModelArrayAnyOfAllOfLinkListColumn1ValueMixin
///
/// Properties:
mixin AllOfModelArrayAnyOfAllOfLinkListColumn1ValueMixin on 
  
  $OpenApiObjectMixin
 {

  UndefinedWrapper<User> get anyOf0;
  UndefinedWrapper<Tag> get anyOf1;
}

/// AllOfModelArrayAnyOfAllOfLinkListColumn1Value
///
/// Properties:
class AllOfModelArrayAnyOfAllOfLinkListColumn1Value with
$OpenApiObjectMixin,


AllOfModelArrayAnyOfAllOfLinkListColumn1ValueMixin {




  @override
  UndefinedWrapper<User> anyOf0;

  @override
  UndefinedWrapper<Tag> anyOf1;


  AllOfModelArrayAnyOfAllOfLinkListColumn1Value.$all({
    
    
    required this.anyOf0,
    required this.anyOf1,
  });

  AllOfModelArrayAnyOfAllOfLinkListColumn1Value({
    
    
    this.anyOf0 = const UndefinedWrapper.undefined(),
    
    this.anyOf1 = const UndefinedWrapper.undefined(),
    
  });
}




