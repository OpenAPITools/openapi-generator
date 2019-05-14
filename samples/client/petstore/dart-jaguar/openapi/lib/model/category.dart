import 'package:jaguar_serializer/jaguar_serializer.dart';


part 'category.jser.dart';

class Category {
  
  @Alias('id')
  final int id;
  
  @Alias('name')
  final String name;
  

  Category(
      

{
     this.id = null,  
     this.name = null 
    
    }
  );

  @override
  String toString() {
    return 'Category[id=$id, name=$name, ]';
  }
}

@GenSerializer(nullableFields: true)
class CategorySerializer extends Serializer<Category> with _$CategorySerializer {

}

