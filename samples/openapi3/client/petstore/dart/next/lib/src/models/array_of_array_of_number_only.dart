// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'array_of_array_of_number_only.reflection.dart';
part 'array_of_array_of_number_only.serialization.dart';


/// ArrayOfArrayOfNumberOnlyMixin
///
/// Properties:
/// * [arrayArrayNumber] 
mixin ArrayOfArrayOfNumberOnlyMixin on 
  
  $OpenApiObjectMixin
 {
  UndefinedWrapper<List<List<num>>> get arrayArrayNumber;

}

/// ArrayOfArrayOfNumberOnly
///
/// Properties:
/// * [arrayArrayNumber] 
class ArrayOfArrayOfNumberOnly with
$OpenApiObjectMixin,


ArrayOfArrayOfNumberOnlyMixin {
  @override
  UndefinedWrapper<List<List<num>>> arrayArrayNumber;





  ArrayOfArrayOfNumberOnly.$all({
    required this.arrayArrayNumber,
    
    
  });

  ArrayOfArrayOfNumberOnly({
    this.arrayArrayNumber = const UndefinedWrapper.undefined(),
    
    
  });
}




