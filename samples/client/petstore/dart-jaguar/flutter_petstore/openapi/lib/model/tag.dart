import 'package:jaguar_serializer/jaguar_serializer.dart';


part 'tag.jser.dart';

class Tag {
  
  @Alias('id', isNullable: false,  )
  final int id;
  
  @Alias('name', isNullable: false,  )
  final String name;
  

  Tag(
      

{
     this.id
     this.name
    
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

