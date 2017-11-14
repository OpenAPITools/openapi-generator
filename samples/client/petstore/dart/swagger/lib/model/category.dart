part of swagger.api;

@Entity()
class Category {
  
  @Property(name: 'id')
  int id = null;
  

  @Property(name: 'name')
  String name = null;
  
  Category();

  @override
  String toString()  {
    return 'Category[id=$id, name=$name, ]';
  }
}

