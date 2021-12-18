//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.12

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

  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  int? id;

  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  Category? category;

  String name;

  Set<String> photoUrls;

  List<Tag> tags;

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
    // ignore: unnecessary_parenthesis
    (id == null ? 0 : id!.hashCode) +
    (category == null ? 0 : category!.hashCode) +
    (name.hashCode) +
    (photoUrls.hashCode) +
    (tags.hashCode) +
    (status == null ? 0 : status!.hashCode);

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
      json[r'tags'] = tags;
    if (status != null) {
      json[r'status'] = status;
    }
    return json;
  }

  /// Returns a new [Pet] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static Pet? fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();

      // Ensure that the map contains the required keys.
      // Note 1: the values aren't checked for validity beyond being non-null.
      // Note 2: this code is stripped in release mode!
      assert(() {
        requiredKeys.forEach((key) {
          assert(json.containsKey(key), 'Required key "Pet[$key]" is missing from JSON.');
          assert(json[key] != null, 'Required key "Pet[$key]" has a null value in JSON.');
        });
        return true;
      }());

      return Pet(
        id: mapValueOfType<int>(json, r'id'),
        category: Category.fromJson(json[r'category']),
        name: mapValueOfType<String>(json, r'name')!,
        photoUrls: json[r'photoUrls'] is Set
            ? (json[r'photoUrls'] as Set).cast<String>()
            : const {},
        tags: Tag.listFromJson(json[r'tags']) ?? const [],
        status: PetStatusEnum.fromJson(json[r'status']),
      );
    }
    return null;
  }

  static List<Pet>? listFromJson(dynamic json, {bool growable = false,}) {
    final result = <Pet>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = Pet.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }

  static Map<String, Pet> mapFromJson(dynamic json) {
    final map = <String, Pet>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = Pet.fromJson(entry.value);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }

  // maps a json object with a list of Pet-objects as value to a dart map
  static Map<String, List<Pet>> mapListFromJson(dynamic json, {bool growable = false,}) {
    final map = <String, List<Pet>>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = Pet.listFromJson(entry.value, growable: growable,);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }

  /// The list of required keys that must be present in a JSON.
  static const requiredKeys = <String>{
    'name',
    'photoUrls',
  };
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

  static PetStatusEnum? fromJson(dynamic value) => PetStatusEnumTypeTransformer().decode(value);

  static List<PetStatusEnum>? listFromJson(dynamic json, {bool growable = false,}) {
    final result = <PetStatusEnum>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = PetStatusEnum.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }
}

/// Transformation class that can [encode] an instance of [PetStatusEnum] to String,
/// and [decode] dynamic data back to [PetStatusEnum].
class PetStatusEnumTypeTransformer {
  factory PetStatusEnumTypeTransformer() => _instance ??= const PetStatusEnumTypeTransformer._();

  const PetStatusEnumTypeTransformer._();

  String encode(PetStatusEnum data) => data.value;

  /// Decodes a [dynamic value][data] to a PetStatusEnum.
  ///
  /// If [allowNull] is true and the [dynamic value][data] cannot be decoded successfully,
  /// then null is returned. However, if [allowNull] is false and the [dynamic value][data]
  /// cannot be decoded successfully, then an [UnimplementedError] is thrown.
  ///
  /// The [allowNull] is very handy when an API changes and a new enum value is added or removed,
  /// and users are still using an old app with the old code.
  PetStatusEnum? decode(dynamic data, {bool allowNull = true}) {
    if (data != null) {
      switch (data.toString()) {
        case r'available': return PetStatusEnum.available;
        case r'pending': return PetStatusEnum.pending;
        case r'sold': return PetStatusEnum.sold;
        default:
          if (!allowNull) {
            throw ArgumentError('Unknown enum value to decode: $data');
          }
      }
    }
    return null;
  }

  /// Singleton [PetStatusEnumTypeTransformer] instance.
  static PetStatusEnumTypeTransformer? _instance;
}


