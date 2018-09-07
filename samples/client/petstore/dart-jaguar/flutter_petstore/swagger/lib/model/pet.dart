import 'package:jaguar_serializer/jaguar_serializer.dart';

import 'package:swagger/model/tag.dart';
import 'package:swagger/model/category.dart';
part 'pet.jser.dart';

class Pet {
  
  final int id;
  
  final Category category;
  
  final String name;
  
  final List<String> photoUrls;
  
  final List<Tag> tags;
   /* pet status in the store */
  final String status;
  //enum statusEnum {  available,  pending,  sold,  };

  Pet(
    

{
     this.id = null,  
     this.category = null,  
    
     this.name = null,  
     this.photoUrls = const [],   this.tags = const [],  
     this.status = null 
    
    }
  );

  @override
  String toString() {
    return 'Pet[id=$id, category=$category, name=$name, photoUrls=$photoUrls, tags=$tags, status=$status, ]';
  }
}

@GenSerializer()
class PetSerializer extends Serializer<Pet> with _$PetSerializer {

}
