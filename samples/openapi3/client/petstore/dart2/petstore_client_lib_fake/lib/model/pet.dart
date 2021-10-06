//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.14

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class Pet {
  /// Returns a new [Pet] instance.
  Pet({
    this.id,
    this.category,
    required this.name,
    this.photoUrls = const {},
    this.tags = const [],
    this.status,
  });


  int? id;

  Category? category;

  String name;

  Set<String> photoUrls;

  List<Tag>? tags;

  /// pet status in the store
  PetStatusEnum? status;

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
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static Pet fromJson(Map<String, dynamic> json) => Pet(
        id: json[r'id'] as int,
        category: Category.fromJson(json[r'category']),
        name: json[r'name'] as String,
        photoUrls: json[r'photoUrls'] is Set
          ? (json[r'photoUrls'] as Set).cast<String>()
          : {},
        tags: Tag.listFromJson(json[r'tags']),
        status: PetStatusEnum.fromJson(json[r'status']),
    );

  static List<Pet> listFromJson(List json, {bool? growable,}) =>
    json.isNotEmpty
      ? json.map<Pet>((i) => Pet.fromJson(i as Map<String, dynamic>)).toList(growable: true == growable)
      : <Pet>[];

  static Map<String, Pet> mapFromJson(dynamic json) {
    final map = <String, Pet>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) => map[key] = Pet.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of Pet-objects as value to a dart map
  static Map<String, List<Pet>> mapListFromJson(dynamic json, {bool? growable,}) {
    final map = <String, List<Pet>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = Pet.listFromJson(
            value,
            growable: growable,
          );
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
  final String? value;

  @override
  String toString() => value ?? '';

  String? toJson() => value;

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

  static List<PetStatusEnum> listFromJson(List json, {bool? growable,}) =>
    json.isNotEmpty
      ? json.map<PetStatusEnum>((i) => PetStatusEnum.fromJson(i as Map<String, dynamic>)).toList(growable: true == growable)
      : <PetStatusEnum>[];
}

/// Transformation class that can [encode] an instance of [PetStatusEnum] to String,
/// and [decode] dynamic data back to [PetStatusEnum].
class PetStatusEnumTypeTransformer {
  factory PetStatusEnumTypeTransformer() => _instance ??= const PetStatusEnumTypeTransformer._();

  const PetStatusEnumTypeTransformer._();

  String? encode(PetStatusEnum data) => data.value;

  /// Decodes a [dynamic value][data] to a PetStatusEnum.
  ///
  /// If the [dynamic value][data] cannot be decoded successfully, then an [UnimplementedError] is thrown.
  PetStatusEnum decode(dynamic data) {
    if (data == r'available') {
      return PetStatusEnum.available;
    }
    if (data == r'pending') {
      return PetStatusEnum.pending;
    }
    if (data == r'sold') {
      return PetStatusEnum.sold;
    }
    throw ArgumentError('Unknown enum value to decode: $data');
  }

  /// Singleton [PetStatusEnumTypeTransformer] instance.
  static PetStatusEnumTypeTransformer? _instance;
}


