//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.18

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class ObjectWithDuplicateInlineEnum {
  /// Returns a new [ObjectWithDuplicateInlineEnum] instance.
  ObjectWithDuplicateInlineEnum({
    this.attribute = const {},
  });

  /// Object two attribute enum
  Set<ObjectWithDuplicateInlineEnumAttributeEnum> attribute;

  @override
  bool operator ==(Object other) => identical(this, other) || other is ObjectWithDuplicateInlineEnum &&
    _deepEquality.equals(other.attribute, attribute);

  @override
  int get hashCode =>
    // ignore: unnecessary_parenthesis
    (attribute.hashCode);

  @override
  String toString() => 'ObjectWithDuplicateInlineEnum[attribute=$attribute]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
      json[r'attribute'] = this.attribute.toList(growable: false);
    return json;
  }

  /// Returns a new [ObjectWithDuplicateInlineEnum] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static ObjectWithDuplicateInlineEnum? fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();

      // Ensure that the map contains the required keys.
      // Note 1: the values aren't checked for validity beyond being non-null.
      // Note 2: this code is stripped in release mode!
      assert(() {
        requiredKeys.forEach((key) {
          assert(json.containsKey(key), 'Required key "ObjectWithDuplicateInlineEnum[$key]" is missing from JSON.');
          assert(json[key] != null, 'Required key "ObjectWithDuplicateInlineEnum[$key]" has a null value in JSON.');
        });
        return true;
      }());

      return ObjectWithDuplicateInlineEnum(
        attribute: ObjectWithDuplicateInlineEnumAttributeEnum.listFromJson(json[r'attribute']).toSet(),
      );
    }
    return null;
  }

  static List<ObjectWithDuplicateInlineEnum> listFromJson(dynamic json, {bool growable = false,}) {
    final result = <ObjectWithDuplicateInlineEnum>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = ObjectWithDuplicateInlineEnum.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }

  static Map<String, ObjectWithDuplicateInlineEnum> mapFromJson(dynamic json) {
    final map = <String, ObjectWithDuplicateInlineEnum>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = ObjectWithDuplicateInlineEnum.fromJson(entry.value);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }

  // maps a json object with a list of ObjectWithDuplicateInlineEnum-objects as value to a dart map
  static Map<String, List<ObjectWithDuplicateInlineEnum>> mapListFromJson(dynamic json, {bool growable = false,}) {
    final map = <String, List<ObjectWithDuplicateInlineEnum>>{};
    if (json is Map && json.isNotEmpty) {
      // ignore: parameter_assignments
      json = json.cast<String, dynamic>();
      for (final entry in json.entries) {
        map[entry.key] = ObjectWithDuplicateInlineEnum.listFromJson(entry.value, growable: growable,);
      }
    }
    return map;
  }

  /// The list of required keys that must be present in a JSON.
  static const requiredKeys = <String>{
  };
}


class ObjectWithDuplicateInlineEnumAttributeEnum {
  /// Instantiate a new enum with the provided [value].
  const ObjectWithDuplicateInlineEnumAttributeEnum._(this.value);

  /// The underlying value of this enum member.
  final String value;

  @override
  String toString() => value;

  String toJson() => value;

  static const valueOne = ObjectWithDuplicateInlineEnumAttributeEnum._(r'value_one');
  static const valueTwo = ObjectWithDuplicateInlineEnumAttributeEnum._(r'value_two');

  /// List of all possible values in this [enum][ObjectWithDuplicateInlineEnumAttributeEnum].
  static const values = <ObjectWithDuplicateInlineEnumAttributeEnum>[
    valueOne,
    valueTwo,
  ];

  static ObjectWithDuplicateInlineEnumAttributeEnum? fromJson(dynamic value) => ObjectWithDuplicateInlineEnumAttributeEnumTypeTransformer().decode(value);

  static List<ObjectWithDuplicateInlineEnumAttributeEnum> listFromJson(dynamic json, {bool growable = false,}) {
    final result = <ObjectWithDuplicateInlineEnumAttributeEnum>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = ObjectWithDuplicateInlineEnumAttributeEnum.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }
}

/// Transformation class that can [encode] an instance of [ObjectWithDuplicateInlineEnumAttributeEnum] to String,
/// and [decode] dynamic data back to [ObjectWithDuplicateInlineEnumAttributeEnum].
class ObjectWithDuplicateInlineEnumAttributeEnumTypeTransformer {
  factory ObjectWithDuplicateInlineEnumAttributeEnumTypeTransformer() => _instance ??= const ObjectWithDuplicateInlineEnumAttributeEnumTypeTransformer._();

  const ObjectWithDuplicateInlineEnumAttributeEnumTypeTransformer._();

  String encode(ObjectWithDuplicateInlineEnumAttributeEnum data) => data.value;

  /// Decodes a [dynamic value][data] to a ObjectWithDuplicateInlineEnumAttributeEnum.
  ///
  /// If [allowNull] is true and the [dynamic value][data] cannot be decoded successfully,
  /// then null is returned. However, if [allowNull] is false and the [dynamic value][data]
  /// cannot be decoded successfully, then an [UnimplementedError] is thrown.
  ///
  /// The [allowNull] is very handy when an API changes and a new enum value is added or removed,
  /// and users are still using an old app with the old code.
  ObjectWithDuplicateInlineEnumAttributeEnum? decode(dynamic data, {bool allowNull = true}) {
    if (data != null) {
      switch (data) {
        case r'value_one': return ObjectWithDuplicateInlineEnumAttributeEnum.valueOne;
        case r'value_two': return ObjectWithDuplicateInlineEnumAttributeEnum.valueTwo;
        default:
          if (!allowNull) {
            throw ArgumentError('Unknown enum value to decode: $data');
          }
      }
    }
    return null;
  }

  /// Singleton [ObjectWithDuplicateInlineEnumAttributeEnumTypeTransformer] instance.
  static ObjectWithDuplicateInlineEnumAttributeEnumTypeTransformer? _instance;
}


