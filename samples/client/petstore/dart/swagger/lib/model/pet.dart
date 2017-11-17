part of swagger.api;

@Entity()
class Pet {
  
  @Property(name: 'id')
  int id = null;
  

  @Property(name: 'category')
  Category category = null;
  

  @Property(name: 'name')
  String name = null;
  

  @Property(name: 'photoUrls')
  List<String> photoUrls = [];
  

  @Property(name: 'tags')
  List<Tag> tags = [];
  
/* pet status in the store */
  @Property(name: 'status')
  String status = null;
  //enum statusEnum {  available,  pending,  sold,  };
  Pet();

  @override
  String toString()  {
    return 'Pet[id=$id, category=$category, name=$name, photoUrls=$photoUrls, tags=$tags, status=$status, ]';
  }
}

