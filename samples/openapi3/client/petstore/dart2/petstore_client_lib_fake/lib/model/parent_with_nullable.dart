//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.12

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class ParentWithNullable {
  /// Returns a new [ParentWithNullable] instance.
  ParentWithNullable({
    this.type,
    this.nullableProperty,
  });

  ParentWithNullableTypeEnum? type;

  String? nullableProperty;

  @override
  bool operator ==(Object other) => identical(this, other) || other is ParentWithNullable &&
    other.type == type &&
    other.nullableProperty == nullableProperty;

  @override
  int get hashCode =>
    // ignore: unnecessary_parenthesis
    (type == null ? 0 : type!.hashCode) +
    (nullableProperty == null ? 0 : nullableProperty!.hashCode);

  @override
  String toString() => 'ParentWithNullable[type=$type, nullableProperty=$nullableProperty]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (this.type != null) {
      json[r'type'] = this.type;
    } else {
      json[r'type'] = null;
    }
    if (this.nullableProperty != null) {
      json[r'nullableProperty'] = this.nullableProperty;
    } else {
      json[r'nullableProperty'] = null;
    }
    return json;
  }

  /// Returns a new [ParentWithNullable] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static ParentWithNullable? fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();

      // Ensure that the map contains the required keys.
      // Note 1: the values aren't checked for validity beyond being non-null.
      // Note 2: this code is stripped in release mode!
      assert(() {
        requiredKeys.forEach((key) {
          assert(json.containsKey(key), 'Required key "ParentWithNullable[$key]" is missing from JSON.');
          assert(json[key] != null, 'Required key "ParentWithNullable[$key]" has a null value in JSON.');
        });
        return true;
      }());

      return ParentWithNullable(
        type: ParentWithNullableTypeEnum.fromJson(json[r'type']),
        nullableProperty: mapValueOfType<String>(json, r'nullableProperty'),
      );
    }
    return null;
  }

  static List<ParentWithNullable> listFromJson(dynamic json, {bool growable = false,}) {
    final result = <ParentWithNullable>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = ParentWithNullable.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }

  static Map<String, ParentWithNullable> mapFromJson(dynamic json) {
    final map = <String, ParentWithNullable>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = ParentWithNullable.fromJson(entry.value);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }

  // maps a json object with a list of ParentWithNullable-objects as value to a dart map
  static Map<String, List<ParentWithNullable>> mapListFromJson(dynamic json, {bool growable = false,}) {
    final map = <String, List<ParentWithNullable>>{};
    if (json is Map && json.isNotEmpty) {
      // ignore: parameter_assignments
      json = json.cast<String, dynamic>();
      for (final entry in json.entries) {
        map[entry.key] = ParentWithNullable.listFromJson(entry.value, growable: growable,);
      }
    }
    return map;
  }

  /// The list of required keys that must be present in a JSON.
  static const requiredKeys = <String>{
  };
}


class ParentWithNullableTypeEnum {
  /// Instantiate a new enum with the provided [value].
  const ParentWithNullableTypeEnum._(this.value);

  /// The underlying value of this enum member.
  final String value;

  @override
  String toString() => value;

  String toJson() => value;

  static const childWithNullable = ParentWithNullableTypeEnum._(r'ChildWithNullable');

  /// List of all possible values in this [enum][ParentWithNullableTypeEnum].
  static const values = <ParentWithNullableTypeEnum>[
    childWithNullable,
  ];

  static ParentWithNullableTypeEnum? fromJson(dynamic value) => ParentWithNullableTypeEnumTypeTransformer().decode(value);

  static List<ParentWithNullableTypeEnum> listFromJson(dynamic json, {bool growable = false,}) {
    final result = <ParentWithNullableTypeEnum>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = ParentWithNullableTypeEnum.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }
}

/// Transformation class that can [encode] an instance of [ParentWithNullableTypeEnum] to String,
/// and [decode] dynamic data back to [ParentWithNullableTypeEnum].
class ParentWithNullableTypeEnumTypeTransformer {
  factory ParentWithNullableTypeEnumTypeTransformer() => _instance ??= const ParentWithNullableTypeEnumTypeTransformer._();

  const ParentWithNullableTypeEnumTypeTransformer._();

  String encode(ParentWithNullableTypeEnum data) => data.value;

  /// Decodes a [dynamic value][data] to a ParentWithNullableTypeEnum.
  ///
  /// If [allowNull] is true and the [dynamic value][data] cannot be decoded successfully,
  /// then null is returned. However, if [allowNull] is false and the [dynamic value][data]
  /// cannot be decoded successfully, then an [UnimplementedError] is thrown.
  ///
  /// The [allowNull] is very handy when an API changes and a new enum value is added or removed,
  /// and users are still using an old app with the old code.
  ParentWithNullableTypeEnum? decode(dynamic data, {bool allowNull = true}) {
    if (data != null) {
      switch (data) {
        case r'ChildWithNullable': return ParentWithNullableTypeEnum.childWithNullable;
        default:
          if (!allowNull) {
            throw ArgumentError('Unknown enum value to decode: $data');
          }
      }
    }
    return null;
  }

  /// Singleton [ParentWithNullableTypeEnumTypeTransformer] instance.
  static ParentWithNullableTypeEnumTypeTransformer? _instance;
}


