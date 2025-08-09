//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.18

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class ObjectWithInlineEnum {
  /// Returns a new [ObjectWithInlineEnum] instance.
  ObjectWithInlineEnum({
    this.attribute = const {},
  });

  /// Object one attribute enum
  Set<ObjectWithInlineEnumAttributeEnum> attribute;

  @override
  bool operator ==(Object other) => identical(this, other) || other is ObjectWithInlineEnum &&
    _deepEquality.equals(other.attribute, attribute);

  @override
  int get hashCode =>
    // ignore: unnecessary_parenthesis
    (attribute.hashCode);

  @override
  String toString() => 'ObjectWithInlineEnum[attribute=$attribute]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
      json[r'attribute'] = this.attribute.toList(growable: false);
    return json;
  }

  /// Returns a new [ObjectWithInlineEnum] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static ObjectWithInlineEnum? fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();

      // Ensure that the map contains the required keys.
      // Note 1: the values aren't checked for validity beyond being non-null.
      // Note 2: this code is stripped in release mode!
      assert(() {
        requiredKeys.forEach((key) {
          assert(json.containsKey(key), 'Required key "ObjectWithInlineEnum[$key]" is missing from JSON.');
          assert(json[key] != null, 'Required key "ObjectWithInlineEnum[$key]" has a null value in JSON.');
        });
        return true;
      }());

      return ObjectWithInlineEnum(
        attribute: ObjectWithInlineEnumAttributeEnum.listFromJson(json[r'attribute']).toSet(),
      );
    }
    return null;
  }

  static List<ObjectWithInlineEnum> listFromJson(dynamic json, {bool growable = false,}) {
    final result = <ObjectWithInlineEnum>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = ObjectWithInlineEnum.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }

  static Map<String, ObjectWithInlineEnum> mapFromJson(dynamic json) {
    final map = <String, ObjectWithInlineEnum>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = ObjectWithInlineEnum.fromJson(entry.value);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }

  // maps a json object with a list of ObjectWithInlineEnum-objects as value to a dart map
  static Map<String, List<ObjectWithInlineEnum>> mapListFromJson(dynamic json, {bool growable = false,}) {
    final map = <String, List<ObjectWithInlineEnum>>{};
    if (json is Map && json.isNotEmpty) {
      // ignore: parameter_assignments
      json = json.cast<String, dynamic>();
      for (final entry in json.entries) {
        map[entry.key] = ObjectWithInlineEnum.listFromJson(entry.value, growable: growable,);
      }
    }
    return map;
  }

  /// The list of required keys that must be present in a JSON.
  static const requiredKeys = <String>{
  };
}


class ObjectWithInlineEnumAttributeEnum {
  /// Instantiate a new enum with the provided [value].
  const ObjectWithInlineEnumAttributeEnum._(this.value);

  /// The underlying value of this enum member.
  final String value;

  @override
  String toString() => value;

  String toJson() => value;

  static const valueOne = ObjectWithInlineEnumAttributeEnum._(r'value_one');
  static const valueTwo = ObjectWithInlineEnumAttributeEnum._(r'value_two');

  /// List of all possible values in this [enum][ObjectWithInlineEnumAttributeEnum].
  static const values = <ObjectWithInlineEnumAttributeEnum>[
    valueOne,
    valueTwo,
  ];

  static ObjectWithInlineEnumAttributeEnum? fromJson(dynamic value) => ObjectWithInlineEnumAttributeEnumTypeTransformer().decode(value);

  static List<ObjectWithInlineEnumAttributeEnum> listFromJson(dynamic json, {bool growable = false,}) {
    final result = <ObjectWithInlineEnumAttributeEnum>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = ObjectWithInlineEnumAttributeEnum.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }
}

/// Transformation class that can [encode] an instance of [ObjectWithInlineEnumAttributeEnum] to String,
/// and [decode] dynamic data back to [ObjectWithInlineEnumAttributeEnum].
class ObjectWithInlineEnumAttributeEnumTypeTransformer {
  factory ObjectWithInlineEnumAttributeEnumTypeTransformer() => _instance ??= const ObjectWithInlineEnumAttributeEnumTypeTransformer._();

  const ObjectWithInlineEnumAttributeEnumTypeTransformer._();

  String encode(ObjectWithInlineEnumAttributeEnum data) => data.value;

  /// Decodes a [dynamic value][data] to a ObjectWithInlineEnumAttributeEnum.
  ///
  /// If [allowNull] is true and the [dynamic value][data] cannot be decoded successfully,
  /// then null is returned. However, if [allowNull] is false and the [dynamic value][data]
  /// cannot be decoded successfully, then an [UnimplementedError] is thrown.
  ///
  /// The [allowNull] is very handy when an API changes and a new enum value is added or removed,
  /// and users are still using an old app with the old code.
  ObjectWithInlineEnumAttributeEnum? decode(dynamic data, {bool allowNull = true}) {
    if (data != null) {
      switch (data) {
        case r'value_one': return ObjectWithInlineEnumAttributeEnum.valueOne;
        case r'value_two': return ObjectWithInlineEnumAttributeEnum.valueTwo;
        default:
          if (!allowNull) {
            throw ArgumentError('Unknown enum value to decode: $data');
          }
      }
    }
    return null;
  }

  /// Singleton [ObjectWithInlineEnumAttributeEnumTypeTransformer] instance.
  static ObjectWithInlineEnumAttributeEnumTypeTransformer? _instance;
}


