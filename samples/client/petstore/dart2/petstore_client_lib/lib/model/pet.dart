part of openapi.api;

class Pet {
  
  int id;
  
  Category category;
  
  String name;
  
  List<String> photoUrls = const [];
  
  List<Tag> tags = const [];
  /// pet status in the store
  PetStatusEnum status;

  Pet({
    this.id,
    this.category,
    @required this.name,
    @required this.photoUrls,
    this.tags = const [],
    this.status,
  });

  @override
  String toString() {
    return 'Pet[id=$id, category=$category, name=$name, photoUrls=$photoUrls, tags=$tags, status=$status, ]';
  }

  Pet.fromJson(Map<String, dynamic> json) {
    if (json == null) return;
    id = json['id'];
    category = (json['category'] == null) ?
      null :
      Category.fromJson(json['category']);
    name = json['name'];
    photoUrls = (json['photoUrls'] == null) ?
      null :
      (json['photoUrls'] as List).cast<String>();
    tags = (json['tags'] == null) ?
      null :
      Tag.listFromJson(json['tags']);
    status = PetStatusEnum.fromJson(json['status']);
  }

  Map<String, dynamic> toJson() {
    Map<String, dynamic> json = {};
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
      json['status'] = status.value;
    return json;
  }

  static List<Pet> listFromJson(List<dynamic> json) {
    return json == null ? List<Pet>() : json.map((value) => Pet.fromJson(value)).toList();
  }

  static Map<String, Pet> mapFromJson(Map<String, dynamic> json) {
    final map = Map<String, Pet>();
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic value) => map[key] = Pet.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of Pet-objects as value to a dart map
  static Map<String, List<Pet>> mapListFromJson(Map<String, dynamic> json) {
    final map = Map<String, List<Pet>>();
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic value) {
        map[key] = Pet.listFromJson(value);
      });
    }
    return map;
  }
}
class PetStatusEnum {
  /// The underlying value of this enum member.
  final String value;

  const PetStatusEnum._internal(this.value);

  /// pet status in the store
  static const PetStatusEnum available_ = PetStatusEnum._internal("available");
  /// pet status in the store
  static const PetStatusEnum pending_ = PetStatusEnum._internal("pending");
  /// pet status in the store
  static const PetStatusEnum sold_ = PetStatusEnum._internal("sold");

  static List<PetStatusEnum> get values => const [
        available_,
        pending_,
        sold_,
      ];

  String toJson () {
    return value;
  }

  @override
  String toString () {
    return value;
  }

  static PetStatusEnum fromJson(String value) {
    return PetStatusEnumTypeTransformer().decode(value);
  }

  static List<PetStatusEnum> listFromJson(List<dynamic> json) {
    return json == null
      ? List<PetStatusEnum>()
      : json.map((value) => PetStatusEnum.fromJson(value)).toList();
  }
}

class PetStatusEnumTypeTransformer {

  dynamic encode(PetStatusEnum data) {
    return data.value;
  }

  PetStatusEnum decode(dynamic data) {
    switch (data) {
      case "available": return PetStatusEnum.available_;
      case "pending": return PetStatusEnum.pending_;
      case "sold": return PetStatusEnum.sold_;
      default: return null;
    }
  }
}


