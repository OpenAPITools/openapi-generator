// Model def

import 'dart:convert';
import 'dart:typed_data';
import 'package:openapi/_internal.dart';
import 'package:xml/xml.dart';

part 'parent_pet.reflection.dart';
part 'parent_pet.serialization.dart';


/// ParentPetMixin
mixin ParentPetMixin on 
  GrandparentAnimalMixin,
  $OpenApiObjectMixin
 {

}

/// ParentPet
class ParentPet with
$OpenApiObjectMixin,

GrandparentAnimalMixin,
ParentPetMixin {
  @override
  String petType;





  ParentPet.$all({
    required this.petType,
    
    
  });

  ParentPet({
  required  this.petType ,
    
    
  });
}




