import 'package:jaguar_serializer/jaguar_serializer.dart';

part 'category.jser.dart';

class Category {
  
  final int id;
  
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

@GenSerializer()
class CategorySerializer extends Serializer<Category> with _$CategorySerializer {

}
