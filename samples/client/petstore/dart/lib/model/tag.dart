part of api;


@Entity()
class Tag {
  
  int id = null;
  
  
  String name = null;
  
  
  Tag();

  @override
  String toString()  {
    return 'Tag[id=$id, name=$name, ]';
  }

}

