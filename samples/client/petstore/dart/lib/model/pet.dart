part of api;


@Entity()
class Pet {
  
  int id = null;
  
  
  Category category = null;
  
  
  String name = null;
  
  
  List<String> photoUrls = [];
  
  
  List<Tag> tags = [];
  
  /* pet status in the store */
  String status = null;
  //enum statusEnum {  available,  pending,  sold,  };
  
  Pet();

  @override
  String toString()  {
    return 'Pet[id=$id, category=$category, name=$name, photoUrls=$photoUrls, tags=$tags, status=$status, ]';
  }

}

