part of swagger.api;

class Pet {
  
  int id = null;
  

  Category category = null;
  

  String name = null;
  

  List<String> photoUrls = [];
  

  List<Tag> tags = [];
  
/* pet status in the store */
  String status = null;
  //enum statusEnum {  available,  pending,  sold,  };
  Pet();

  @override
  String toString() {
    return 'Pet[id=$id, category=$category, name=$name, photoUrls=$photoUrls, tags=$tags, status=$status, ]';
  }

  Pet.fromJson(Map<String, dynamic> json) {
    if (json == null) return;
    id =
    json['id'];
    category =
        
        
        new Category.fromJson(json['category'])
    ;
    name =
    json['name'];
    photoUrls =
    json['photoUrls'];
    tags =
        Tag.listFromJson(json['tags'])
    ;
    status =
    json['status'];
  }

  Map<String, dynamic> toJson() {
    return {
      'id': id,
      'category': category,
      'name': name,
      'photoUrls': photoUrls,
      'tags': tags,
      'status': status
     };
  }

  static List<Pet> listFromJson(List<Map<String, dynamic>> json) {
    var list = new List<Pet>();
    if (json != null && json.length > 0) {
      json.forEach((Map<String, dynamic> value) => list.add(new Pet.fromJson(value)));
    }
    return list;
  }

  static Map<String, Pet> mapFromJson(Map<String, Map<String, dynamic>> json) {
    var map = new Map<String, Pet>();
    if (json != null && json.length > 0) {
      json.forEach((String key, Map<String, dynamic> value) => map[key] = new Pet.fromJson(value));
    }
    return map;
  }
}

