//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.12

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class ChildWithNullable {
  /// Returns a new [ChildWithNullable] instance.
  ChildWithNullable({
    this.type,
    this.nullableProperty,
    this.otherProperty,
  });

  ChildWithNullableTypeEnum? type;

  String? nullableProperty;

  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  String? otherProperty;

  @override
  bool operator ==(Object other) => identical(this, other) || other is ChildWithNullable &&
    other.type == type &&
    other.nullableProperty == nullableProperty &&
    other.otherProperty == otherProperty;

  @override
  int get hashCode =>
    // ignore: unnecessary_parenthesis
    (type == null ? 0 : type!.hashCode) +
    (nullableProperty == null ? 0 : nullableProperty!.hashCode) +
    (otherProperty == null ? 0 : otherProperty!.hashCode);

  @override
  String toString() => 'ChildWithNullable[type=$type, nullableProperty=$nullableProperty, otherProperty=$otherProperty]';

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
    if (this.otherProperty != null) {
      json[r'otherProperty'] = this.otherProperty;
    } else {
      json[r'otherProperty'] = null;
    }
    return json;
  }

  /// Returns a new [ChildWithNullable] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static ChildWithNullable? fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();

      // Ensure that the map contains the required keys.
      // Note 1: the values aren't checked for validity beyond being non-null.
      // Note 2: this code is stripped in release mode!
      assert(() {
        requiredKeys.forEach((key) {
          assert(json.containsKey(key), 'Required key "ChildWithNullable[$key]" is missing from JSON.');
          assert(json[key] != null, 'Required key "ChildWithNullable[$key]" has a null value in JSON.');
        });
        return true;
      }());

      return ChildWithNullable(
        type: ChildWithNullableTypeEnum.fromJson(json[r'type']),
        nullableProperty: mapValueOfType<String>(json, r'nullableProperty'),
        otherProperty: mapValueOfType<String>(json, r'otherProperty'),
      );
    }
    return null;
  }

  static List<ChildWithNullable> listFromJson(dynamic json, {bool growable = false,}) {
    final result = <ChildWithNullable>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = ChildWithNullable.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }

  static Map<String, ChildWithNullable> mapFromJson(dynamic json) {
    final map = <String, ChildWithNullable>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = ChildWithNullable.fromJson(entry.value);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }

  // maps a json object with a list of ChildWithNullable-objects as value to a dart map
  static Map<String, List<ChildWithNullable>> mapListFromJson(dynamic json, {bool growable = false,}) {
    final map = <String, List<ChildWithNullable>>{};
    if (json is Map && json.isNotEmpty) {
      // ignore: parameter_assignments
      json = json.cast<String, dynamic>();
      for (final entry in json.entries) {
        map[entry.key] = ChildWithNullable.listFromJson(entry.value, growable: growable,);
      }
    }
    return map;
  }

  /// The list of required keys that must be present in a JSON.
  static const requiredKeys = <String>{
  };
}


class ChildWithNullableTypeEnum {
  /// Instantiate a new enum with the provided [value].
  const ChildWithNullableTypeEnum._(this.value);

  /// The underlying value of this enum member.
  final String value;

  @override
  String toString() => value;

  String toJson() => value;

  static const childWithNullable = ChildWithNullableTypeEnum._(r'ChildWithNullable');

  /// List of all possible values in this [enum][ChildWithNullableTypeEnum].
  static const values = <ChildWithNullableTypeEnum>[
    childWithNullable,
  ];

  static ChildWithNullableTypeEnum? fromJson(dynamic value) => ChildWithNullableTypeEnumTypeTransformer().decode(value);

  static List<ChildWithNullableTypeEnum> listFromJson(dynamic json, {bool growable = false,}) {
    final result = <ChildWithNullableTypeEnum>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = ChildWithNullableTypeEnum.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }
}

/// Transformation class that can [encode] an instance of [ChildWithNullableTypeEnum] to String,
/// and [decode] dynamic data back to [ChildWithNullableTypeEnum].
class ChildWithNullableTypeEnumTypeTransformer {
  factory ChildWithNullableTypeEnumTypeTransformer() => _instance ??= const ChildWithNullableTypeEnumTypeTransformer._();

  const ChildWithNullableTypeEnumTypeTransformer._();

  String encode(ChildWithNullableTypeEnum data) => data.value;

  /// Decodes a [dynamic value][data] to a ChildWithNullableTypeEnum.
  ///
  /// If [allowNull] is true and the [dynamic value][data] cannot be decoded successfully,
  /// then null is returned. However, if [allowNull] is false and the [dynamic value][data]
  /// cannot be decoded successfully, then an [UnimplementedError] is thrown.
  ///
  /// The [allowNull] is very handy when an API changes and a new enum value is added or removed,
  /// and users are still using an old app with the old code.
  ChildWithNullableTypeEnum? decode(dynamic data, {bool allowNull = true}) {
    if (data != null) {
      switch (data) {
        case r'ChildWithNullable': return ChildWithNullableTypeEnum.childWithNullable;
        default:
          if (!allowNull) {
            throw ArgumentError('Unknown enum value to decode: $data');
          }
      }
    }
    return null;
  }

  /// Singleton [ChildWithNullableTypeEnumTypeTransformer] instance.
  static ChildWithNullableTypeEnumTypeTransformer? _instance;
}


