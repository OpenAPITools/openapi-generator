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
    (id == null ? 0 : id.hashCode) +
    (category == null ? 0 : category.hashCode) +
    (name == null ? 0 : name.hashCode) +
    (photoUrls == null ? 0 : photoUrls.hashCode) +
    (tags == null ? 0 : tags.hashCode) +
    (status == null ? 0 : status.hashCode);

  @override
  String toString() => 'Pet[id=$id, category=$category, name=$name, photoUrls=$photoUrls, tags=$tags, status=$status]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (id != null) {
      json[r'id'] = id;
    }
    if (category != null) {
      json[r'category'] = category;
    }
    if (name != null) {
      json[r'name'] = name;
    }
    if (photoUrls != null) {
      json[r'photoUrls'] = photoUrls;
    }
    if (tags != null) {
      json[r'tags'] = tags;
    }
    if (status != null) {
      json[r'status'] = status;
    }
    return json;
  }

  /// Returns a new [Pet] instance and imports its values from
  /// [json] if it's non-null, null if [json] is null.
  static Pet fromJson(Map<String, dynamic> json) => json == null
    ? null
    : Pet(
        id: json[r'id'],
        category: _$enumDecode(_$CategoryEnumMap, json[r'category']),
        name: json[r'name'],
        photoUrls: json[r'photoUrls'] == null
          ? null
          : (json[r'photoUrls'] as List).cast<String>(),
        tags: Tag.listFromJson(json[r'tags']),
        status: _$enumDecode(_$PetStatusEnum, json[r'status']),
    );

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
enum PetStatusEnum {
        available,
        pending,
        sold,
}

const _$PetStatusEnum = <PetStatusEnum, dynamic>{
        PetStatusEnum.available: 'available',
        PetStatusEnum.pending: 'pending',
        PetStatusEnum.sold: 'sold',
};


