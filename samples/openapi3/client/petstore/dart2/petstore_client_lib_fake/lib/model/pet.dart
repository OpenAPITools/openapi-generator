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
    this.photoUrls = const {},
    this.tags = const [],
    this.status,
  });

  int id;

  Category category;

  String name;

  Set<String> photoUrls;

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
      json[r'name'] = name;
      json[r'photoUrls'] = photoUrls;
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
        category: Category.fromJson(json[r'category']),
        name: json[r'name'],
        photoUrls: json[r'photoUrls'] == null
          ? null
          : (json[r'photoUrls'] as Set).cast<String>(),
        tags: Tag.listFromJson(json[r'tags']),
        status: PetStatusEnum.fromJson(json[r'status']),
    );

  static List<Pet> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <Pet>[]
      : json.map((dynamic value) => Pet.fromJson(value)).toList(growable: true == growable);

  static Map<String, Pet> mapFromJson(Map<String, dynamic> json) {
    final map = <String, Pet>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) => map[key] = Pet.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of Pet-objects as value to a dart map
  static Map<String, List<Pet>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<Pet>>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) {
        map[key] = Pet.listFromJson(value, emptyIsNull: emptyIsNull, growable: growable,);
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
  String toString() => value;

  String toJson() => value;

  static const available = PetStatusEnum._(r'available');
  static const pending = PetStatusEnum._(r'pending');
  static const sold = PetStatusEnum._(r'sold');

  /// List of all possible values in this [enum][PetStatusEnum].
  static const values = <PetStatusEnum>[
    available,
    pending,
    sold,
  ];

  static PetStatusEnum fromJson(dynamic value) =>
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
      case r'available': return PetStatusEnum.available;
      case r'pending': return PetStatusEnum.pending;
      case r'sold': return PetStatusEnum.sold;
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

