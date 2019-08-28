import 'package:jaguar_serializer/jaguar_serializer.dart';


import 'package:openapi/model/tag.dart';

import 'package:openapi/model/category.dart';

part 'pet.jser.dart';

class Pet {
  
  @Alias('id', isNullable: false,  )
  final int id;
  
  @Alias('category', isNullable: false,  )
  final Category category;
  
  @Alias('name', isNullable: false,  )
  final String name;
  
  @Alias('photoUrls', isNullable: false,  )
  final List<String> photoUrls;
  
  @Alias('tags', isNullable: false,  )
  final List<Tag> tags;
   /* pet status in the store */
  @Alias('status', isNullable: false,
          
  )
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

@GenSerializer(nullableFields: true)
class PetSerializer extends Serializer<Pet> with _$PetSerializer {

}

