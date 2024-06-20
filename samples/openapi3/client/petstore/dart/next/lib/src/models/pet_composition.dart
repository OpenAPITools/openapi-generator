// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'pet_composition.reflection.dart';
part 'pet_composition.serialization.dart';


//class defination

///
mixin PetCompositionMixin on 
  PetMixin,
  $OpenApiObjectMixin
 {


}

///
class PetComposition with
$OpenApiObjectMixin,

PetMixin,
PetCompositionMixin {
  @override
  List<String> photoUrls;
  @override
  String name;
  @override
  UndefinedWrapper<int> id;
  @override
  UndefinedWrapper<Category> category;
  @override
  UndefinedWrapper<List<Tag>> tags;
  @override
  UndefinedWrapper<StatusEnum> status;





  PetComposition.$all({
    required this.photoUrls,
    required this.name,
    required this.id,
    required this.category,
    required this.tags,
    required this.status,
    
    
  });

  PetComposition({
  required  this.photoUrls ,
  required  this.name ,
    this.id = const UndefinedWrapper.undefined(),
    this.category = const UndefinedWrapper.undefined(),
    this.tags = const UndefinedWrapper.undefined(),
    this.status = const UndefinedWrapper.undefined(),
    
    
  });
}




