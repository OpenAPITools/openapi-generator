// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'number_only.reflection.dart';
part 'number_only.serialization.dart';


/// NumberOnlyMixin
///
/// Properties:
/// * [justNumber] 
mixin NumberOnlyMixin on 
  
  $OpenApiObjectMixin
 {
  UndefinedWrapper<num> get justNumber;

}

/// NumberOnly
///
/// Properties:
/// * [justNumber] 
class NumberOnly with
$OpenApiObjectMixin,


NumberOnlyMixin {
  @override
  UndefinedWrapper<num> justNumber;





  NumberOnly.$all({
    required this.justNumber,
    
    
  });

  NumberOnly({
    this.justNumber = const UndefinedWrapper.undefined(),
    
    
  });
}




