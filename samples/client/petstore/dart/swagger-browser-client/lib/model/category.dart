part of swagger.api;

class Category {
  
  int id = null;
  

  String name = null;
  
  Category();

  @override
  String toString() {
    return 'Category[id=$id, name=$name, ]';
  }

  Category.fromJson(Map<String, dynamic> json) {
    if (json == null) return;
    id =
    json['id'];
    name =
    json['name'];
  }

  Map<String, dynamic> toJson() {
    return {
      'id': id,
      'name': name
     };
  }

  static List<Category> listFromJson(List<Map<String, dynamic>> json) {
    var list = new List<Category>();
    if (json != null && json.length > 0) {
      json.forEach((Map<String, dynamic> value) => list.add(new Category.fromJson(value)));
    }
    return list;
  }

  static Map<String, Category> mapFromJson(Map<String, Map<String, dynamic>> json) {
    var map = new Map<String, Category>();
    if (json != null && json.length > 0) {
      json.forEach((String key, Map<String, dynamic> value) => map[key] = new Category.fromJson(value));
    }
    return map;
  }
}

