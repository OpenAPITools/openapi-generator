//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.12

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class SpecialCat {
  /// Returns a new [SpecialCat] instance.
  SpecialCat({
    required this.className,
    this.color = 'red',
    this.declawed,
    this.kind,
  });

  String className;

  String color;

  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  bool? declawed;

  SpecialCatKindEnum? kind;

  @override
  bool operator ==(Object other) => identical(this, other) || other is SpecialCat &&
     other.className == className &&
     other.color == color &&
     other.declawed == declawed &&
     other.kind == kind;

  @override
  int get hashCode =>
    // ignore: unnecessary_parenthesis
    (className.hashCode) +
    (color.hashCode) +
    (declawed == null ? 0 : declawed!.hashCode) +
    (kind == null ? 0 : kind!.hashCode);

  @override
  String toString() => 'SpecialCat[className=$className, color=$color, declawed=$declawed, kind=$kind]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
      json[r'className'] = this.className;
      json[r'color'] = this.color;
    if (this.declawed != null) {
      json[r'declawed'] = this.declawed;
    } else {
      json[r'declawed'] = null;
    }
    if (this.kind != null) {
      json[r'kind'] = this.kind;
    } else {
      json[r'kind'] = null;
    }
    return json;
  }

  /// Returns a new [SpecialCat] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static SpecialCat? fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();

      // Ensure that the map contains the required keys.
      // Note 1: the values aren't checked for validity beyond being non-null.
      // Note 2: this code is stripped in release mode!
      assert(() {
        requiredKeys.forEach((key) {
          assert(json.containsKey(key), 'Required key "SpecialCat[$key]" is missing from JSON.');
          assert(json[key] != null, 'Required key "SpecialCat[$key]" has a null value in JSON.');
        });
        return true;
      }());

      return SpecialCat(
        className: mapValueOfType<String>(json, r'className')!,
        color: mapValueOfType<String>(json, r'color') ?? 'red',
        declawed: mapValueOfType<bool>(json, r'declawed'),
        kind: SpecialCatKindEnum.fromJson(json[r'kind']),
      );
    }
    return null;
  }

  static List<SpecialCat>? listFromJson(dynamic json, {bool growable = false,}) {
    final result = <SpecialCat>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = SpecialCat.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }

  static Map<String, SpecialCat> mapFromJson(dynamic json) {
    final map = <String, SpecialCat>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = SpecialCat.fromJson(entry.value);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }

  // maps a json object with a list of SpecialCat-objects as value to a dart map
  static Map<String, List<SpecialCat>> mapListFromJson(dynamic json, {bool growable = false,}) {
    final map = <String, List<SpecialCat>>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = SpecialCat.listFromJson(entry.value, growable: growable,);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }

  /// The list of required keys that must be present in a JSON.
  static const requiredKeys = <String>{
    'className',
  };
}


class SpecialCatKindEnum {
  /// Instantiate a new enum with the provided [value].
  const SpecialCatKindEnum._(this.value);

  /// The underlying value of this enum member.
  final String value;

  @override
  String toString() => value;

  String toJson() => value;

  static const lions = SpecialCatKindEnum._(r'lions');
  static const tigers = SpecialCatKindEnum._(r'tigers');
  static const leopards = SpecialCatKindEnum._(r'leopards');
  static const jaguars = SpecialCatKindEnum._(r'jaguars');

  /// List of all possible values in this [enum][SpecialCatKindEnum].
  static const values = <SpecialCatKindEnum>[
    lions,
    tigers,
    leopards,
    jaguars,
  ];

  static SpecialCatKindEnum? fromJson(dynamic value) => SpecialCatKindEnumTypeTransformer().decode(value);

  static List<SpecialCatKindEnum>? listFromJson(dynamic json, {bool growable = false,}) {
    final result = <SpecialCatKindEnum>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = SpecialCatKindEnum.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }
}

/// Transformation class that can [encode] an instance of [SpecialCatKindEnum] to String,
/// and [decode] dynamic data back to [SpecialCatKindEnum].
class SpecialCatKindEnumTypeTransformer {
  factory SpecialCatKindEnumTypeTransformer() => _instance ??= const SpecialCatKindEnumTypeTransformer._();

  const SpecialCatKindEnumTypeTransformer._();

  String encode(SpecialCatKindEnum data) => data.value;

  /// Decodes a [dynamic value][data] to a SpecialCatKindEnum.
  ///
  /// If [allowNull] is true and the [dynamic value][data] cannot be decoded successfully,
  /// then null is returned. However, if [allowNull] is false and the [dynamic value][data]
  /// cannot be decoded successfully, then an [UnimplementedError] is thrown.
  ///
  /// The [allowNull] is very handy when an API changes and a new enum value is added or removed,
  /// and users are still using an old app with the old code.
  SpecialCatKindEnum? decode(dynamic data, {bool allowNull = true}) {
    if (data != null) {
      switch (data) {
        case r'lions': return SpecialCatKindEnum.lions;
        case r'tigers': return SpecialCatKindEnum.tigers;
        case r'leopards': return SpecialCatKindEnum.leopards;
        case r'jaguars': return SpecialCatKindEnum.jaguars;
        default:
          if (!allowNull) {
            throw ArgumentError('Unknown enum value to decode: $data');
          }
      }
    }
    return null;
  }

  /// Singleton [SpecialCatKindEnumTypeTransformer] instance.
  static SpecialCatKindEnumTypeTransformer? _instance;
}


