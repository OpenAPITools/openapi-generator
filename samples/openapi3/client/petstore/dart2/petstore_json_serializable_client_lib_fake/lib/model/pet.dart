//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.14

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

@JsonSerializable(
  checked: true,
  createToJson: true,
  disallowUnrecognizedKeys: true,
  explicitToJson: true,
)
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


  @JsonKey(
    name: r'id',
    required: false,
  )
  int? id;

  @JsonKey(
    name: r'category',
    required: false,
  )
  Category? category;

  @JsonKey(
    name: r'name',
    required: true,
  )
  String name;

  @JsonKey(
    defaultValue: const {},
    name: r'photoUrls',
    required: true,
  )
  Set<String> photoUrls;

  @JsonKey(
    defaultValue: const [],
    name: r'tags',
    required: false,
  )
  List<Tag>? tags;

  /// pet status in the store
  @JsonKey(
    name: r'status',
    required: false,
  )
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

  factory Pet.fromJson(Map<String, dynamic> json) => _$PetFromJson(json);

  Map<String, dynamic> toJson() => _$PetToJson(this);

  @override
  String toString() => toJson().toString();
}

/// pet status in the store
enum PetStatusEnum {
  available,
  pending,
  sold,
}

