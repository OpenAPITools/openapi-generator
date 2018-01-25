part of swagger.api;

class Tag {
  
  int id = null;
  

  String name = null;
  
  Tag();

  @override
  String toString() {
    return 'Tag[id=$id, name=$name, ]';
  }

  Tag.fromJson(Map<String, dynamic> json) {
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

  static List<Tag> listFromJson(List<Map<String, dynamic>> json) {
    var list = new List<Tag>();
    if (json != null && json.length > 0) {
      json.forEach((Map<String, dynamic> value) => list.add(new Tag.fromJson(value)));
    }
    return list;
  }

  static Map<String, Tag> mapFromJson(Map<String, Map<String, dynamic>> json) {
    var map = new Map<String, Tag>();
    if (json != null && json.length > 0) {
      json.forEach((String key, Map<String, dynamic> value) => map[key] = new Tag.fromJson(value));
    }
    return map;
  }
}

