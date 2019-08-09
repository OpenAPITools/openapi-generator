part of openapi.api;

class Pet {
  
  int id = null;
  
  Category category = null;
  
  String name = null;
  
  List<String> photoUrls = [];
  
  List<Tag> tags = [];
  /* pet status in the store */
  String status = null;
  //enum statusEnum {  available,  pending,  sold,  };{
  Pet();

  @override
  String toString() {
    return 'Pet[id=$id, category=$category, name=$name, photoUrls=$photoUrls, tags=$tags, status=$status, ]';
  }

  Pet.fromJson(Map<String, dynamic> json) {
    if (json == null) return;
    if (json['id'] == null) {
      id = null;
    } else {
          id = json['id'];
    }
    if (json['category'] == null) {
      category = null;
    } else {
      category = new Category.fromJson(json['category']);
    }
    if (json['name'] == null) {
      name = null;
    } else {
          name = json['name'];
    }
    if (json['photoUrls'] == null) {
      photoUrls = null;
    } else {
      photoUrls = (json['photoUrls'] as List).cast<String>();
    }
    if (json['tags'] == null) {
      tags = null;
    } else {
      tags = Tag.listFromJson(json['tags']);
    }
    if (json['status'] == null) {
      status = null;
    } else {
          status = json['status'];
    }
  }

  Map<String, dynamic> toJson() {
    Map <String, dynamic> json = {};
    if (id != null)
      json['id'] = id;
    if (category != null)
      json['category'] = category;
    if (name != null)
      json['name'] = name;
    if (photoUrls != null)
      json['photoUrls'] = photoUrls;
    if (tags != null)
      json['tags'] = tags;
    if (status != null)
      json['status'] = status;
    return json;
  }

  static List<Pet> listFromJson(List<dynamic> json) {
    return json == null ? new List<Pet>() : json.map((value) => new Pet.fromJson(value)).toList();
  }

  static Map<String, Pet> mapFromJson(Map<String, dynamic> json) {
    var map = new Map<String, Pet>();
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic value) => map[key] = new Pet.fromJson(value));
    }
    return map;
  }
}

