part of openapi.api;

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
    if (json['id'] == null) {
      id = null;
    } else {
      id = json['id'];
    }
    if (json['name'] == null) {
      name = null;
    } else {
      name = json['name'];
    }
  }

  Map<String, dynamic> toJson() {
    return {
      'id': id,
      'name': name
    };
  }

  static List<Category> listFromJson(List<dynamic> json) {
    return json == null ? new List<Category>() : json.map((value) => new Category.fromJson(value)).toList();
  }

  static Map<String, Category> mapFromJson(Map<String, Map<String, dynamic>> json) {
    var map = new Map<String, Category>();
    if (json != null && json.length > 0) {
      json.forEach((String key, Map<String, dynamic> value) => map[key] = new Category.fromJson(value));
    }
    return map;
  }
}

