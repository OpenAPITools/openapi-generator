// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'all_of_model_array_any_of_all_of_attributes.reflection.dart';
part 'all_of_model_array_any_of_all_of_attributes.serialization.dart';


/// AllOfModelArrayAnyOfAllOfAttributesMixin
///
/// Properties:
/// * [C] 
mixin AllOfModelArrayAnyOfAllOfAttributesMixin on 
  
  $OpenApiObjectMixin
 {
  UndefinedWrapper<AllOfModelArrayAnyOfAllOfAttributesC> get C;

}

/// AllOfModelArrayAnyOfAllOfAttributes
///
/// Properties:
/// * [C] 
class AllOfModelArrayAnyOfAllOfAttributes with
$OpenApiObjectMixin,


AllOfModelArrayAnyOfAllOfAttributesMixin {
  @override
  UndefinedWrapper<AllOfModelArrayAnyOfAllOfAttributesC> C;





  AllOfModelArrayAnyOfAllOfAttributes.$all({
    required this.C,
    
    
  });

  AllOfModelArrayAnyOfAllOfAttributes({
    this.C = const UndefinedWrapper.undefined(),
    
    
  });
}




