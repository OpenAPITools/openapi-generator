part of swagger.api;

@Entity()
class Tag {
  
  @Property(name: 'id')
  int id = null;
  

  @Property(name: 'name')
  String name = null;
  
  Tag();

  @override
  String toString()  {
    return 'Tag[id=$id, name=$name, ]';
  }
}

