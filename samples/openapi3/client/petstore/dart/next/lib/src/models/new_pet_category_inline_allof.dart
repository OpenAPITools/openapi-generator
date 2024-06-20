// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'new_pet_category_inline_allof.reflection.dart';
part 'new_pet_category_inline_allof.serialization.dart';


/// NewPetCategoryInlineAllofMixin
///
/// Properties:
/// * [id] 
/// * [name] 
/// * [categoryTag] 
mixin NewPetCategoryInlineAllofMixin on 
  
  $OpenApiObjectMixin
 {
  UndefinedWrapper<int> get id;
  String get name;
  UndefinedWrapper<NewPetCategoryInlineAllofAllOfCategoryTag> get categoryTag;

}

/// NewPetCategoryInlineAllof
///
/// Properties:
/// * [id] 
/// * [name] 
/// * [categoryTag] 
class NewPetCategoryInlineAllof with
$OpenApiObjectMixin,


NewPetCategoryInlineAllofMixin {
  @override
  UndefinedWrapper<int> id;
  @override
  String name;
  @override
  UndefinedWrapper<NewPetCategoryInlineAllofAllOfCategoryTag> categoryTag;





  NewPetCategoryInlineAllof.$all({
    required this.id,
    required this.name,
    required this.categoryTag,
    
    
  });

  NewPetCategoryInlineAllof({
    this.id = const UndefinedWrapper.undefined(),
    this.name = 'default-name',
    this.categoryTag = const UndefinedWrapper.undefined(),
    
    
  });
}




