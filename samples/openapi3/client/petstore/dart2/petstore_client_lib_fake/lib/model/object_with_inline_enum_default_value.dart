//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.18

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class ObjectWithInlineEnumDefaultValue {
  /// Returns a new [ObjectWithInlineEnumDefaultValue] instance.
  ObjectWithInlineEnumDefaultValue({
    this.attribute = const ObjectWithInlineEnumDefaultValueAttributeEnum._('value_one'),
  });

  /// Object one attribute enum with default value
  ObjectWithInlineEnumDefaultValueAttributeEnum attribute;

  @override
  bool operator ==(Object other) => identical(this, other) || other is ObjectWithInlineEnumDefaultValue &&
    other.attribute == attribute;

  @override
  int get hashCode =>
    // ignore: unnecessary_parenthesis
    (attribute.hashCode);

  @override
  String toString() => 'ObjectWithInlineEnumDefaultValue[attribute=$attribute]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
      json[r'attribute'] = this.attribute;
    return json;
  }

  /// Returns a new [ObjectWithInlineEnumDefaultValue] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static ObjectWithInlineEnumDefaultValue? fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();

      // Ensure that the map contains the required keys.
      // Note 1: the values aren't checked for validity beyond being non-null.
      // Note 2: this code is stripped in release mode!
      assert(() {
        requiredKeys.forEach((key) {
          assert(json.containsKey(key), 'Required key "ObjectWithInlineEnumDefaultValue[$key]" is missing from JSON.');
          assert(json[key] != null, 'Required key "ObjectWithInlineEnumDefaultValue[$key]" has a null value in JSON.');
        });
        return true;
      }());

      return ObjectWithInlineEnumDefaultValue(
        attribute: ObjectWithInlineEnumDefaultValueAttributeEnum.fromJson(json[r'attribute']) ?? 'value_one',
      );
    }
    return null;
  }

  static List<ObjectWithInlineEnumDefaultValue> listFromJson(dynamic json, {bool growable = false,}) {
    final result = <ObjectWithInlineEnumDefaultValue>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = ObjectWithInlineEnumDefaultValue.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }

  static Map<String, ObjectWithInlineEnumDefaultValue> mapFromJson(dynamic json) {
    final map = <String, ObjectWithInlineEnumDefaultValue>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = ObjectWithInlineEnumDefaultValue.fromJson(entry.value);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }

  // maps a json object with a list of ObjectWithInlineEnumDefaultValue-objects as value to a dart map
  static Map<String, List<ObjectWithInlineEnumDefaultValue>> mapListFromJson(dynamic json, {bool growable = false,}) {
    final map = <String, List<ObjectWithInlineEnumDefaultValue>>{};
    if (json is Map && json.isNotEmpty) {
      // ignore: parameter_assignments
      json = json.cast<String, dynamic>();
      for (final entry in json.entries) {
        map[entry.key] = ObjectWithInlineEnumDefaultValue.listFromJson(entry.value, growable: growable,);
      }
    }
    return map;
  }

  /// The list of required keys that must be present in a JSON.
  static const requiredKeys = <String>{
  };
}

/// Object one attribute enum with default value
class ObjectWithInlineEnumDefaultValueAttributeEnum {
  /// Instantiate a new enum with the provided [value].
  const ObjectWithInlineEnumDefaultValueAttributeEnum._(this.value);

  /// The underlying value of this enum member.
  final String value;

  @override
  String toString() => value;

  String toJson() => value;

  static const valueOne = ObjectWithInlineEnumDefaultValueAttributeEnum._(r'value_one');
  static const valueTwo = ObjectWithInlineEnumDefaultValueAttributeEnum._(r'value_two');

  /// List of all possible values in this [enum][ObjectWithInlineEnumDefaultValueAttributeEnum].
  static const values = <ObjectWithInlineEnumDefaultValueAttributeEnum>[
    valueOne,
    valueTwo,
  ];

  static ObjectWithInlineEnumDefaultValueAttributeEnum? fromJson(dynamic value) => ObjectWithInlineEnumDefaultValueAttributeEnumTypeTransformer().decode(value);

  static List<ObjectWithInlineEnumDefaultValueAttributeEnum> listFromJson(dynamic json, {bool growable = false,}) {
    final result = <ObjectWithInlineEnumDefaultValueAttributeEnum>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = ObjectWithInlineEnumDefaultValueAttributeEnum.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }
}

/// Transformation class that can [encode] an instance of [ObjectWithInlineEnumDefaultValueAttributeEnum] to String,
/// and [decode] dynamic data back to [ObjectWithInlineEnumDefaultValueAttributeEnum].
class ObjectWithInlineEnumDefaultValueAttributeEnumTypeTransformer {
  factory ObjectWithInlineEnumDefaultValueAttributeEnumTypeTransformer() => _instance ??= const ObjectWithInlineEnumDefaultValueAttributeEnumTypeTransformer._();

  const ObjectWithInlineEnumDefaultValueAttributeEnumTypeTransformer._();

  String encode(ObjectWithInlineEnumDefaultValueAttributeEnum data) => data.value;

  /// Decodes a [dynamic value][data] to a ObjectWithInlineEnumDefaultValueAttributeEnum.
  ///
  /// If [allowNull] is true and the [dynamic value][data] cannot be decoded successfully,
  /// then null is returned. However, if [allowNull] is false and the [dynamic value][data]
  /// cannot be decoded successfully, then an [UnimplementedError] is thrown.
  ///
  /// The [allowNull] is very handy when an API changes and a new enum value is added or removed,
  /// and users are still using an old app with the old code.
  ObjectWithInlineEnumDefaultValueAttributeEnum? decode(dynamic data, {bool allowNull = true}) {
    if (data != null) {
      switch (data) {
        case r'value_one': return ObjectWithInlineEnumDefaultValueAttributeEnum.valueOne;
        case r'value_two': return ObjectWithInlineEnumDefaultValueAttributeEnum.valueTwo;
        default:
          if (!allowNull) {
            throw ArgumentError('Unknown enum value to decode: $data');
          }
      }
    }
    return null;
  }

  /// Singleton [ObjectWithInlineEnumDefaultValueAttributeEnumTypeTransformer] instance.
  static ObjectWithInlineEnumDefaultValueAttributeEnumTypeTransformer? _instance;
}


