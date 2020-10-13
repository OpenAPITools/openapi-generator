//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class Pet {
  /// Returns a new [Pet] instance.
  Pet({
    this.id,
    this.category,
    @required this.name,
    this.photoUrls = const [],
    this.tags = const [],
    this.status,
  });

  /// Returns a new [Pet] instance and optionally import its values from
  /// [json] if it's non-null.
  Pet.fromJson(Map<String, dynamic> json) {
    if (json != null) {
      id = json['id'];
      category = Category.fromJson(json['category']);
      name = json['name'];
      photoUrls = json['photoUrls'] == null
        ? null
        : (json['photoUrls'] as List).cast<String>();
      tags = Tag.listFromJson(json['tags']);
      status = PetStatusEnum.fromJson(json['status']);
    }
  }

  
  int id;

  
  Category category;

  
  String name;

  
  List<String> photoUrls;

  
  List<Tag> tags;

  /// pet status in the store
  PetStatusEnum status;

  @override
  bool operator ==(Object other) => identical(this, other) || other is Pet &&
     other.id == id &&
     other.category == category &&
     other.name == name &&
     other.photoUrls == photoUrls &&
     other.tags == tags &&
     other.status == status;

  @override
  int get hashCode =>
    id.hashCode +
    category.hashCode +
    name.hashCode +
    photoUrls.hashCode +
    tags.hashCode +
    status.hashCode;

  @override
  String toString() => 'Pet[id=$id, category=$category, name=$name, photoUrls=$photoUrls, tags=$tags, status=$status]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (id != null) {
      json['id'] = id;
    }
    if (category != null) {
      json['category'] = category;
    }
    if (name != null) {
      json['name'] = name;
    }
    if (photoUrls != null) {
      json['photoUrls'] = photoUrls;
    }
    if (tags != null) {
      json['tags'] = tags;
    }
    if (status != null) {
      json['status'] = status;
    }
    return json;
  }

  static List<Pet> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <Pet>[]
      : json.map((v) => Pet.fromJson(v)).toList(growable: true == growable);

  static Map<String, Pet> mapFromJson(Map<String, dynamic> json) {
    final map = <String, Pet>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) => map[key] = Pet.fromJson(v));
    }
    return map;
  }

  // maps a json object with a list of Pet-objects as value to a dart map
  static Map<String, List<Pet>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<Pet>>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) {
        map[key] = Pet.listFromJson(v, emptyIsNull: emptyIsNull, growable: growable);
      });
    }
    return map;
  }
}

/// pet status in the store
class PetStatusEnum {
  /// Instantiate a new enum with the provided [value].
  const PetStatusEnum._(this.value);

  /// The underlying value of this enum member.
  final String value;

  @override
  bool operator ==(Object other) => identical(this, other) ||
      other is PetStatusEnum && other.value == value ||
      other is String && other == value;

  @override
  int get hashCode => toString().hashCode;

  @override
  String toString() => value;

  String toJson() => value;

  static const available_ = PetStatusEnum._('available');
  static const pending_ = PetStatusEnum._('pending');
  static const sold_ = PetStatusEnum._('sold');

  /// List of all possible values in this [enum][PetStatusEnum].
  static const values = <PetStatusEnum>[
    available_,
    pending_,
    sold_,
  ];

  static PetStatusEnum fromJson(String value) =>
    PetStatusEnumTypeTransformer().decode(value);

  static List<PetStatusEnum> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <PetStatusEnum>[]
      : json
          .map((value) => PetStatusEnum.fromJson(value))
          .toList(growable: true == growable);
}

/// Transformation class that can [encode] an instance of [PetStatusEnum] to String,
/// and [decode] dynamic data back to [PetStatusEnum].
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
      case 'available': return PetStatusEnum.available_;
      case 'pending': return PetStatusEnum.pending_;
      case 'sold': return PetStatusEnum.sold_;
      default:
        if (allowNull == false) {
          throw ArgumentError('Unknown enum value to decode: $data');
        }
    }
    return null;
  }

  /// Singleton [PetStatusEnumTypeTransformer] instance.
  static PetStatusEnumTypeTransformer _instance;
}

