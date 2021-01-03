//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

@JsonSerializable(
  checked: true,
  includeIfNull: false,
  disallowUnrecognizedKeys: true,
)
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

  @JsonKey(
    name: r'id',
    
    
    
  )
  int id;

  @JsonKey(
    name: r'category',
    
    
    
  )
  Category category;

  @JsonKey(
    name: r'name',
    required: true,
    
    
  )
  String name;

  @JsonKey(
    name: r'photoUrls',
    required: true,
    defaultValue: const [],
    
  )
  List<String> photoUrls;

  @JsonKey(
    name: r'tags',
    
    defaultValue: const [],
    
  )
  List<Tag> tags;

  /// pet status in the store
  @JsonKey(
    name: r'status',
    
    
    
  )
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
  String toString() => toJson().toString();

  factory Pet.fromJson(Map<String, dynamic> json) => _$PetFromJson(json);
  Map<String, dynamic> toJson() => _$PetToJson(this);
}

/// pet status in the store
enum PetStatusEnum {

    @JsonValue(r'available')
    
    available,
    @JsonValue(r'pending')
    
    pending,
    @JsonValue(r'sold')
    
    sold,

}


