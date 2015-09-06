part of api;


@Entity()
class Category {
  
  int id = null;
  
  
  String name = null;
  
  
  Category();

  @override
  String toString()  {
    return 'Category[id=$id, name=$name, ]';
  }

}

