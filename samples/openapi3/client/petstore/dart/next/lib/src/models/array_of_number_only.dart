// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'array_of_number_only.reflection.dart';
part 'array_of_number_only.serialization.dart';


/// ArrayOfNumberOnlyMixin
///
/// Properties:
/// * [arrayNumber] 
mixin ArrayOfNumberOnlyMixin on 
  
  $OpenApiObjectMixin
 {
  UndefinedWrapper<List<num>> get arrayNumber;

}

/// ArrayOfNumberOnly
///
/// Properties:
/// * [arrayNumber] 
class ArrayOfNumberOnly with
$OpenApiObjectMixin,


ArrayOfNumberOnlyMixin {
  @override
  UndefinedWrapper<List<num>> arrayNumber;





  ArrayOfNumberOnly.$all({
    required this.arrayNumber,
    
    
  });

  ArrayOfNumberOnly({
    this.arrayNumber = const UndefinedWrapper.undefined(),
    
    
  });
}




