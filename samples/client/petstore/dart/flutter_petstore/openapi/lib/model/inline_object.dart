part of openapi.api;

class InlineObject {
  /* Updated name of the pet */
  String name = null;
  /* Updated status of the pet */
  String status = null;
  InlineObject();

  @override
  String toString() {
    return 'InlineObject[name=$name, status=$status, ]';
  }

  InlineObject.fromJson(Map<String, dynamic> json) {
    if (json == null) return;
    if (json['name'] == null) {
      name = null;
    } else {
      name = json['name'];
    }
    if (json['status'] == null) {
      status = null;
    } else {
      status = json['status'];
    }
  }

  Map<String, dynamic> toJson() {
    return {
          'name': name,
          'status': status
    };
  }

  static List<InlineObject> listFromJson(List<dynamic> json) {
    return json == null ? new List<InlineObject>() : json.map((value) => new InlineObject.fromJson(value)).toList();
  }

  static Map<String, InlineObject> mapFromJson(Map<String, Map<String, dynamic>> json) {
    var map = new Map<String, InlineObject>();
    if (json != null && json.length > 0) {
      json.forEach((String key, Map<String, dynamic> value) => map[key] = new InlineObject.fromJson(value));
    }
    return map;
  }
}

