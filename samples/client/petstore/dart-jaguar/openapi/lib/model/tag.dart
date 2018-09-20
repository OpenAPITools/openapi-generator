import 'package:jaguar_serializer/jaguar_serializer.dart';

part 'tag.jser.dart';

class Tag {
  
  final int id;
  
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

@GenSerializer()
class TagSerializer extends Serializer<Tag> with _$TagSerializer {

}
