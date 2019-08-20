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
    Map <String, dynamic> json = {};
    if (id != null)
      json['id'] = id;
    if (name != null)
      json['name'] = name;
    return json;
  }

  static List<Tag> listFromJson(List<dynamic> json) {
    return json == null ? List<Tag>() : json.map((value) => Tag.fromJson(value)).toList();
  }

  static Map<String, Tag> mapFromJson(Map<String, dynamic> json) {
    var map = Map<String, Tag>();
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic value) => map[key] = Tag.fromJson(value));
    }
    return map;
  }
}

