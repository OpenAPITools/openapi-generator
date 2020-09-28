//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element
// ignore_for_file: always_put_required_named_parameters_first

part of openapi.api;

class Pet {
  Pet({
    this.id,
    this.category,
    @required this.name,
    @required this.photoUrls,
    this.tags,
    this.status,
  });

  
  int id;

  
  Category category;

  
  String name;

  
  List<String> photoUrls = const [];

  
  List<Tag> tags = const [];

  /// pet status in the store
  PetStatusEnum status;

  @override
  String toString() => 'Pet[id=$id, category=$category, name=$name, photoUrls=$photoUrls, tags=$tags, status=$status, ]';

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
    final json = <String, dynamic>{};
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

  static List<Pet> listFromJson(List<dynamic> json, {bool growable}) =>
    json == null
      ? <Pet>[]
      : json.map((v) => Pet.fromJson(v)).toList(growable: true == growable);

  static Map<String, Pet> mapFromJson(Map<String, dynamic> json) {
    final map = <String, Pet>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) => map[key] = Pet.fromJson(v));
    }
    return map;
  }

  // maps a json object with a list of Pet-objects as value to a dart map
  static Map<String, List<Pet>> mapListFromJson(Map<String, dynamic> json, {bool growable}) {
    final map = <String, List<Pet>>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) {
        map[key] = Pet.listFromJson(v, growable: growable);
      });
    }
    return map;
  }
}

/// Possible values for the [PetStatusEnum] enum.
abstract class PetStatusEnumValue {
  /// Disable instantiation.
  const PetStatusEnumValue._();


  /// pet status in the store
  static const available_ = "available";

  /// pet status in the store
  static const pending_ = "pending";

  /// pet status in the store
  static const sold_ = "sold";
}

class PetStatusEnum {
  /// Instantiate a new enum with the provided [value].
  const PetStatusEnum._(this.value);

  /// The underlying value of this enum member.
  final String value;

  @override
  String toString() => value.toString();

  String toJson() => value;

  /// pet status in the store
  static const PetStatusEnum available_ = PetStatusEnum._(PetStatusEnumValue.available_);

  /// pet status in the store
  static const PetStatusEnum pending_ = PetStatusEnum._(PetStatusEnumValue.pending_);

  /// pet status in the store
  static const PetStatusEnum sold_ = PetStatusEnum._(PetStatusEnumValue.sold_);

  /// List of all possible values in this [enum][PetStatusEnum].
  static const values = <PetStatusEnum>[
    available_,
    pending_,
    sold_,
  ];

  static PetStatusEnum fromJson(String value) =>
    PetStatusEnumTypeTransformer().decode(value);

  static List<PetStatusEnum> listFromJson(List<dynamic> json, {bool growable}) =>
    json == null
      ? <PetStatusEnum>[]
      : json
          .map((value) => PetStatusEnum.fromJson(value))
          .toList(growable: true == growable);
}

class PetStatusEnumTypeTransformer {
  const PetStatusEnumTypeTransformer._();

  factory PetStatusEnumTypeTransformer() => _instance ??= PetStatusEnumTypeTransformer._();

  String encode(PetStatusEnum data) => data.value;

  /// Decodes a [dynamic value][data] to a PetStatusEnum.
  ///
  /// If [allowNull] is true and the [dynamic value][data] cannot be decoded successfully,
  /// then null is returned. However, if [allowNull] is false and the [dynamic value][data]
  /// cannot be decoded successfully, then an [UnimplementedError] is thrown.
  ///
  /// The [allowNull] is very handy when an API changes and a new enum value is added or removed,
  /// and users are still using an old app with the old code.
  PetStatusEnum decode(dynamic data, {bool allowNull}) {
    switch (data) {
      case PetStatusEnumValue.available_: return PetStatusEnum.available_;
      case PetStatusEnumValue.pending_: return PetStatusEnum.pending_;
      case PetStatusEnumValue.sold_: return PetStatusEnum.sold_;
      default:
        if (false == allowNull) {
          throw UnimplementedError('Unknown enum value to decode: $data');
        }
    }
    return null;
  }

  /// Singleton [PetStatusEnumTypeTransformer] instance.
  static PetStatusEnumTypeTransformer _instance;
}


