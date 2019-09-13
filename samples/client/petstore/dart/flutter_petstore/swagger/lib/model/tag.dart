part of openapi.api;

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

  static List<Tag> listFromJson(List<dynamic> json) {
    return json == null ? new List<Tag>() : json.map((value) => new Tag.fromJson(value)).toList();
  }

  static Map<String, Tag> mapFromJson(Map<String, Map<String, dynamic>> json) {
    var map = new Map<String, Tag>();
    if (json != null && json.length > 0) {
      json.forEach((String key, Map<String, dynamic> value) => map[key] = new Tag.fromJson(value));
    }
    return map;
  }
}

