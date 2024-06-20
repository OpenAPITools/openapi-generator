// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'has_only_read_only.reflection.dart';
part 'has_only_read_only.serialization.dart';


/// HasOnlyReadOnlyMixin
///
/// Properties:
/// * [bar] 
/// * [foo] 
mixin HasOnlyReadOnlyMixin on 
  
  $OpenApiObjectMixin
 {
  UndefinedWrapper<String> get bar;
  UndefinedWrapper<String> get foo;

}

/// HasOnlyReadOnly
///
/// Properties:
/// * [bar] 
/// * [foo] 
class HasOnlyReadOnly with
$OpenApiObjectMixin,


HasOnlyReadOnlyMixin {
  @override
  UndefinedWrapper<String> bar;
  @override
  UndefinedWrapper<String> foo;





  HasOnlyReadOnly.$all({
    required this.bar,
    required this.foo,
    
    
  });

  HasOnlyReadOnly({
    this.bar = const UndefinedWrapper.undefined(),
    this.foo = const UndefinedWrapper.undefined(),
    
    
  });
}




