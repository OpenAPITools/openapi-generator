part of openapi.api;

class InlineObject {
  /// Updated name of the pet
  String name;
  /// Updated status of the pet
  String status;

  InlineObject({
    this.name,
    this.status,
  });

  @override
  String toString() {
    return 'InlineObject[name=$name, status=$status, ]';
  }

  InlineObject.fromJson(Map<String, dynamic> json) {
    if (json == null) return;
    name = json['name'];
    status = json['status'];
    if (false)
      throw ArgumentError("$json has not all required fields for InlineObject");
  }

  Map<String, dynamic> toJson() {
    Map<String, dynamic> json = {};
    if (name != null)
      json['name'] = name;
    if (status != null)
      json['status'] = status;
    return json;
  }

  static List<InlineObject> listFromJson(List<dynamic> json) {
    return json == null ? List<InlineObject>() : json.map((value) => InlineObject.fromJson(value)).toList();
  }

  static Map<String, InlineObject> mapFromJson(Map<String, dynamic> json) {
    final map = Map<String, InlineObject>();
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic value) => map[key] = InlineObject.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of InlineObject-objects as value to a dart map
  static Map<String, List<InlineObject>> mapListFromJson(Map<String, dynamic> json) {
    final map = Map<String, List<InlineObject>>();
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic value) {
        map[key] = InlineObject.listFromJson(value);
      });
    }
    return map;
  }
}

