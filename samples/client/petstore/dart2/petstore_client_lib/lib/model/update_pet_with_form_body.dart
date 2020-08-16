part of openapi.api;

class UpdatePetWithFormBody {
  /// Updated name of the pet
  String name;
  /// Updated status of the pet
  String status;

  UpdatePetWithFormBody({
    this.name,
    this.status,
  });

  @override
  String toString() {
    return 'UpdatePetWithFormBody[name=$name, status=$status, ]';
  }

  UpdatePetWithFormBody.fromJson(Map<String, dynamic> json) {
    if (json == null) return;
    name = json['name'];
    status = json['status'];
  }

  Map<String, dynamic> toJson() {
    Map<String, dynamic> json = {};
    if (name != null)
      json['name'] = name;
    if (status != null)
      json['status'] = status;
    return json;
  }

  static List<UpdatePetWithFormBody> listFromJson(List<dynamic> json) {
    return json == null ? List<UpdatePetWithFormBody>() : json.map((value) => UpdatePetWithFormBody.fromJson(value)).toList();
  }

  static Map<String, UpdatePetWithFormBody> mapFromJson(Map<String, dynamic> json) {
    final map = Map<String, UpdatePetWithFormBody>();
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic value) => map[key] = UpdatePetWithFormBody.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of UpdatePetWithFormBody-objects as value to a dart map
  static Map<String, List<UpdatePetWithFormBody>> mapListFromJson(Map<String, dynamic> json) {
    final map = Map<String, List<UpdatePetWithFormBody>>();
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic value) {
        map[key] = UpdatePetWithFormBody.listFromJson(value);
      });
    }
    return map;
  }
}

