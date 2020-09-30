//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: avoid_init_to_null, lines_longer_than_80_chars
// ignore_for_file: prefer_single_quotes

part of openapi.api;

/// [String] values for all properties defined in [Pet].
abstract class PetStrings {
  const PetStrings._();

  static const id_ = "id";
  static const category_ = "category";
  static const name_ = "name";
  static const photoUrls_ = "photoUrls";
  static const tags_ = "tags";
  static const status_ = "status";
}

class Pet {
  Pet({
    this.id,
    this.category,
    @required this.name,
    this.photoUrls = const const [],
    this.tags = const const [],
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
    id.hashCode +
    category.hashCode +
    name.hashCode +
    photoUrls.hashCode +
    tags.hashCode +
    status.hashCode;

  @override
  String toString() => _toString("");

  Pet.fromJson(Map<String, dynamic> json) {
    if (json == null) {
      return;
    }
    id = json[PetStrings.id_];
    category = Category.fromJson(json[PetStrings.category_]);
    name = json[PetStrings.name_];
    photoUrls = json[PetStrings.photoUrls_] == null ?
      null :
      (json[PetStrings.photoUrls_] as List).cast<String>();
    tags = Tag.listFromJson(json[PetStrings.tags_]);
    status = PetStatusEnum.fromJson(json[PetStrings.status_]);
  }

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (id != null) {
      json[PetStrings.id_] = id;
    }
    if (category != null) {
      json[PetStrings.category_] = category;
    }
    if (name != null) {
      json[PetStrings.name_] = name;
    }
    if (photoUrls != null) {
      json[PetStrings.photoUrls_] = photoUrls;
    }
    if (tags != null) {
      json[PetStrings.tags_] = tags;
    }
    if (status != null) {
      json[PetStrings.status_] = status;
    }
    return json;
  }

  String _toString(String prefix) {
    final sb = StringBuffer();

    sb.write("Pet=[");

    sb.write("\n$prefix  ");
    sb.write(PetStrings.id_);
    sb.write(": ");
    sb.write(id);
  sb.write(",");

    sb.write("\n$prefix  ");
    sb.write(PetStrings.category_);
    sb.write(": ");
    sb.write(category == null ? "null" : category._toString("$prefix  "));
  sb.write(",");

    sb.write("\n$prefix  ");
    sb.write(PetStrings.name_);
    sb.write(": ");
    sb.write(name);
  sb.write(",");

    sb.write("\n$prefix  ");
    sb.write(PetStrings.photoUrls_);
    sb.write(": ");
    sb.write(photoUrls);
  sb.write(",");

    sb.write("\n$prefix  ");
    sb.write(PetStrings.tags_);
    sb.write(": ");
    sb.write(tags);
  sb.write(",");

    sb.write("\n$prefix  ");
    sb.write(PetStrings.status_);
    sb.write(": ");
    sb.write(status);
  

    sb.write("\n$prefix]");

    return sb.toString();
  }

  static List<Pet> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable}) =>
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
  static Map<String, List<Pet>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable}) {
    final map = <String, List<Pet>>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) {
        map[key] = Pet.listFromJson(v, emptyIsNull: emptyIsNull, growable: growable);
      });
    }
    return map;
  }
}

/// [String] values for all enums defined in [Pet].
abstract class PetStrings {
  const PetStrings._();

  static const available_ = "available";
  static const pending_ = "pending";
  static const sold_ = "sold";
}

/// pet status in the store
class PetStatusEnum {
  /// Instantiate a new enum with the provided [value].
  const PetStatusEnum._(this.value);

  /// The underlying value of this enum member.
  final String value;

  @override
  bool operator ==(Object other) =>
      identical(this, other) ||
      other is PetStatusEnum && other.value == value ||
      other is String && other == value;

  @override
  int get hashCode => toString().hashCode;

  @override
  String toString() => value;

  String toJson() => toString();

  String _toString(String _) => toString();

  static const available_ = PetStatusEnum._(PetStatusEnumStrings.available_);
  static const pending_ = PetStatusEnum._(PetStatusEnumStrings.pending_);
  static const sold_ = PetStatusEnum._(PetStatusEnumStrings.sold_);

  /// List of all possible values in this [enum][PetStatusEnum].
  static const values = <PetStatusEnum>[
    available_,
    pending_,
    sold_,
  ];

  static PetStatusEnum fromJson(String value) =>
    PetStatusEnumTypeTransformer().decode(value);

  static List<PetStatusEnum> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable}) =>
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
      case PetStatusEnumStrings.available_:
        return PetStatusEnum.available_;
      case PetStatusEnumStrings.pending_:
        return PetStatusEnum.pending_;
      case PetStatusEnumStrings.sold_:
        return PetStatusEnum.sold_;
      default:
        if (false == allowNull) {
          throw ArgumentError("Unknown enum value to decode: $data");
        }
    }
    return null;
  }

  /// Singleton [PetStatusEnumTypeTransformer] instance.
  static PetStatusEnumTypeTransformer _instance;
}

