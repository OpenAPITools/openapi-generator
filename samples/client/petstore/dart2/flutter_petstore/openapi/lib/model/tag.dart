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
    id = json['id'];
    name = json['name'];
  }

  Map<String, dynamic> toJson() {
    return {'id': id, 'name': name};
  }

  static List<Tag> listFromJson(List<dynamic> json) {
    return json == null
        ? new List<Tag>()
        : json.map((value) => new Tag.fromJson(value)).toList();
  }

  static Map<String, Tag> mapFromJson(Map<String, dynamic> json) {
    var map = new Map<String, Tag>();
    if (json != null && json.length > 0) {
      json.forEach(
          (String key, dynamic value) => map[key] = new Tag.fromJson(value));
    }
    return map;
  }
}
