import 'package:jaguar_serializer/jaguar_serializer.dart';


part 'tag.jser.dart';

class Tag {
  
  @Alias('id')
  final int id;
  
  @Alias('name')
  final String name;
  

  Tag(
      

{
     this.id = null,  
     this.name = null 
    
    }
  );

  @override
  String toString() {
    return 'Tag[id=$id, name=$name, ]';
  }
}

@GenSerializer(nullableFields: true)
class TagSerializer extends Serializer<Tag> with _$TagSerializer {

}

