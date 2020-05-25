part of openapi.api;

class UpdatePetWithFormBody {
  /* Updated name of the pet */
  String name = null;
  /* Updated status of the pet */
  String status = null;
  UpdatePetWithFormBody();

  @override
  String toString() {
    return 'UpdatePetWithFormBody[name=$name, status=$status, ]';
  }

  UpdatePetWithFormBody.fromJson(Map<String, dynamic> json) {
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

  static List<UpdatePetWithFormBody> listFromJson(List<dynamic> json) {
    return json == null ? new List<UpdatePetWithFormBody>() : json.map((value) => new UpdatePetWithFormBody.fromJson(value)).toList();
  }

  static Map<String, UpdatePetWithFormBody> mapFromJson(Map<String, Map<String, dynamic>> json) {
    var map = new Map<String, UpdatePetWithFormBody>();
    if (json != null && json.length > 0) {
      json.forEach((String key, Map<String, dynamic> value) => map[key] = new UpdatePetWithFormBody.fromJson(value));
    }
    return map;
  }
}

