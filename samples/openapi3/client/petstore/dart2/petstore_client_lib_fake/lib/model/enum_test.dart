//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.12

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class EnumTest {
  /// Returns a new [EnumTest] instance.
  EnumTest({
    this.enumString,
    required this.enumStringRequired,
    this.enumInteger,
    this.enumNumber,
    this.outerEnum,
    this.outerEnumInteger,
    this.outerEnumDefaultValue,
    this.outerEnumIntegerDefaultValue,
  });

  EnumTestEnumStringEnum? enumString;

  EnumTestEnumStringRequiredEnum enumStringRequired;

  EnumTestEnumIntegerEnum? enumInteger;

  EnumTestEnumNumberEnum? enumNumber;

  OuterEnum? outerEnum;

  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  OuterEnumInteger? outerEnumInteger;

  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  OuterEnumDefaultValue? outerEnumDefaultValue;

  ///
  /// Please note: This property should have been non-nullable! Since the specification file
  /// does not include a default value (using the "default:" property), however, the generated
  /// source code must fall back to having a nullable type.
  /// Consider adding a "default:" property in the specification file to hide this note.
  ///
  OuterEnumIntegerDefaultValue? outerEnumIntegerDefaultValue;

  @override
  bool operator ==(Object other) => identical(this, other) || other is EnumTest &&
     other.enumString == enumString &&
     other.enumStringRequired == enumStringRequired &&
     other.enumInteger == enumInteger &&
     other.enumNumber == enumNumber &&
     other.outerEnum == outerEnum &&
     other.outerEnumInteger == outerEnumInteger &&
     other.outerEnumDefaultValue == outerEnumDefaultValue &&
     other.outerEnumIntegerDefaultValue == outerEnumIntegerDefaultValue;

  @override
  int get hashCode =>
    // ignore: unnecessary_parenthesis
    (enumString == null ? 0 : enumString!.hashCode) +
    (enumStringRequired.hashCode) +
    (enumInteger == null ? 0 : enumInteger!.hashCode) +
    (enumNumber == null ? 0 : enumNumber!.hashCode) +
    (outerEnum == null ? 0 : outerEnum!.hashCode) +
    (outerEnumInteger == null ? 0 : outerEnumInteger!.hashCode) +
    (outerEnumDefaultValue == null ? 0 : outerEnumDefaultValue!.hashCode) +
    (outerEnumIntegerDefaultValue == null ? 0 : outerEnumIntegerDefaultValue!.hashCode);

  @override
  String toString() => 'EnumTest[enumString=$enumString, enumStringRequired=$enumStringRequired, enumInteger=$enumInteger, enumNumber=$enumNumber, outerEnum=$outerEnum, outerEnumInteger=$outerEnumInteger, outerEnumDefaultValue=$outerEnumDefaultValue, outerEnumIntegerDefaultValue=$outerEnumIntegerDefaultValue]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (enumString != null) {
      json[r'enum_string'] = enumString;
    }
      json[r'enum_string_required'] = enumStringRequired;
    if (enumInteger != null) {
      json[r'enum_integer'] = enumInteger;
    }
    if (enumNumber != null) {
      json[r'enum_number'] = enumNumber;
    }
    if (outerEnum != null) {
      json[r'outerEnum'] = outerEnum;
    }
    if (outerEnumInteger != null) {
      json[r'outerEnumInteger'] = outerEnumInteger;
    }
    if (outerEnumDefaultValue != null) {
      json[r'outerEnumDefaultValue'] = outerEnumDefaultValue;
    }
    if (outerEnumIntegerDefaultValue != null) {
      json[r'outerEnumIntegerDefaultValue'] = outerEnumIntegerDefaultValue;
    }
    return json;
  }

  /// Returns a new [EnumTest] instance and imports its values from
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static EnumTest? fromJson(dynamic value) {
    if (value is Map) {
      final json = value.cast<String, dynamic>();

      // Ensure that the map contains the required keys.
      // Note 1: the values aren't checked for validity beyond being non-null.
      // Note 2: this code is stripped in release mode!
      assert(() {
        requiredKeys.forEach((key) {
          assert(json.containsKey(key), 'Required key "EnumTest[$key]" is missing from JSON.');
          assert(json[key] != null, 'Required key "EnumTest[$key]" has a null value in JSON.');
        });
        return true;
      }());

      return EnumTest(
        enumString: EnumTestEnumStringEnum.fromJson(json[r'enum_string']),
        enumStringRequired: EnumTestEnumStringRequiredEnum.fromJson(json[r'enum_string_required'])!,
        enumInteger: EnumTestEnumIntegerEnum.fromJson(json[r'enum_integer']),
        enumNumber: EnumTestEnumNumberEnum.fromJson(json[r'enum_number']),
        outerEnum: OuterEnum.fromJson(json[r'outerEnum']),
        outerEnumInteger: OuterEnumInteger.fromJson(json[r'outerEnumInteger']),
        outerEnumDefaultValue: OuterEnumDefaultValue.fromJson(json[r'outerEnumDefaultValue']),
        outerEnumIntegerDefaultValue: OuterEnumIntegerDefaultValue.fromJson(json[r'outerEnumIntegerDefaultValue']),
      );
    }
    return null;
  }

  static List<EnumTest>? listFromJson(dynamic json, {bool growable = false,}) {
    final result = <EnumTest>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = EnumTest.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }

  static Map<String, EnumTest> mapFromJson(dynamic json) {
    final map = <String, EnumTest>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = EnumTest.fromJson(entry.value);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }

  // maps a json object with a list of EnumTest-objects as value to a dart map
  static Map<String, List<EnumTest>> mapListFromJson(dynamic json, {bool growable = false,}) {
    final map = <String, List<EnumTest>>{};
    if (json is Map && json.isNotEmpty) {
      json = json.cast<String, dynamic>(); // ignore: parameter_assignments
      for (final entry in json.entries) {
        final value = EnumTest.listFromJson(entry.value, growable: growable,);
        if (value != null) {
          map[entry.key] = value;
        }
      }
    }
    return map;
  }

  /// The list of required keys that must be present in a JSON.
  static const requiredKeys = <String>{
    'enum_string_required',
  };
}


class EnumTestEnumStringEnum {
  /// Instantiate a new enum with the provided [value].
  const EnumTestEnumStringEnum._(this.value);

  /// The underlying value of this enum member.
  final String value;

  @override
  String toString() => value;

  String toJson() => value;

  static const UPPER = EnumTestEnumStringEnum._(r'UPPER');
  static const lower = EnumTestEnumStringEnum._(r'lower');
  static const empty = EnumTestEnumStringEnum._(r'');

  /// List of all possible values in this [enum][EnumTestEnumStringEnum].
  static const values = <EnumTestEnumStringEnum>[
    UPPER,
    lower,
    empty,
  ];

  static EnumTestEnumStringEnum? fromJson(dynamic value) => EnumTestEnumStringEnumTypeTransformer().decode(value);

  static List<EnumTestEnumStringEnum>? listFromJson(dynamic json, {bool growable = false,}) {
    final result = <EnumTestEnumStringEnum>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = EnumTestEnumStringEnum.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }
}

/// Transformation class that can [encode] an instance of [EnumTestEnumStringEnum] to String,
/// and [decode] dynamic data back to [EnumTestEnumStringEnum].
class EnumTestEnumStringEnumTypeTransformer {
  factory EnumTestEnumStringEnumTypeTransformer() => _instance ??= const EnumTestEnumStringEnumTypeTransformer._();

  const EnumTestEnumStringEnumTypeTransformer._();

  String encode(EnumTestEnumStringEnum data) => data.value;

  /// Decodes a [dynamic value][data] to a EnumTestEnumStringEnum.
  ///
  /// If [allowNull] is true and the [dynamic value][data] cannot be decoded successfully,
  /// then null is returned. However, if [allowNull] is false and the [dynamic value][data]
  /// cannot be decoded successfully, then an [UnimplementedError] is thrown.
  ///
  /// The [allowNull] is very handy when an API changes and a new enum value is added or removed,
  /// and users are still using an old app with the old code.
  EnumTestEnumStringEnum? decode(dynamic data, {bool allowNull = true}) {
    if (data != null) {
      switch (data.toString()) {
        case r'UPPER': return EnumTestEnumStringEnum.UPPER;
        case r'lower': return EnumTestEnumStringEnum.lower;
        case r'': return EnumTestEnumStringEnum.empty;
        default:
          if (!allowNull) {
            throw ArgumentError('Unknown enum value to decode: $data');
          }
      }
    }
    return null;
  }

  /// Singleton [EnumTestEnumStringEnumTypeTransformer] instance.
  static EnumTestEnumStringEnumTypeTransformer? _instance;
}



class EnumTestEnumStringRequiredEnum {
  /// Instantiate a new enum with the provided [value].
  const EnumTestEnumStringRequiredEnum._(this.value);

  /// The underlying value of this enum member.
  final String value;

  @override
  String toString() => value;

  String toJson() => value;

  static const UPPER = EnumTestEnumStringRequiredEnum._(r'UPPER');
  static const lower = EnumTestEnumStringRequiredEnum._(r'lower');
  static const empty = EnumTestEnumStringRequiredEnum._(r'');

  /// List of all possible values in this [enum][EnumTestEnumStringRequiredEnum].
  static const values = <EnumTestEnumStringRequiredEnum>[
    UPPER,
    lower,
    empty,
  ];

  static EnumTestEnumStringRequiredEnum? fromJson(dynamic value) => EnumTestEnumStringRequiredEnumTypeTransformer().decode(value);

  static List<EnumTestEnumStringRequiredEnum>? listFromJson(dynamic json, {bool growable = false,}) {
    final result = <EnumTestEnumStringRequiredEnum>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = EnumTestEnumStringRequiredEnum.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }
}

/// Transformation class that can [encode] an instance of [EnumTestEnumStringRequiredEnum] to String,
/// and [decode] dynamic data back to [EnumTestEnumStringRequiredEnum].
class EnumTestEnumStringRequiredEnumTypeTransformer {
  factory EnumTestEnumStringRequiredEnumTypeTransformer() => _instance ??= const EnumTestEnumStringRequiredEnumTypeTransformer._();

  const EnumTestEnumStringRequiredEnumTypeTransformer._();

  String encode(EnumTestEnumStringRequiredEnum data) => data.value;

  /// Decodes a [dynamic value][data] to a EnumTestEnumStringRequiredEnum.
  ///
  /// If [allowNull] is true and the [dynamic value][data] cannot be decoded successfully,
  /// then null is returned. However, if [allowNull] is false and the [dynamic value][data]
  /// cannot be decoded successfully, then an [UnimplementedError] is thrown.
  ///
  /// The [allowNull] is very handy when an API changes and a new enum value is added or removed,
  /// and users are still using an old app with the old code.
  EnumTestEnumStringRequiredEnum? decode(dynamic data, {bool allowNull = true}) {
    if (data != null) {
      switch (data.toString()) {
        case r'UPPER': return EnumTestEnumStringRequiredEnum.UPPER;
        case r'lower': return EnumTestEnumStringRequiredEnum.lower;
        case r'': return EnumTestEnumStringRequiredEnum.empty;
        default:
          if (!allowNull) {
            throw ArgumentError('Unknown enum value to decode: $data');
          }
      }
    }
    return null;
  }

  /// Singleton [EnumTestEnumStringRequiredEnumTypeTransformer] instance.
  static EnumTestEnumStringRequiredEnumTypeTransformer? _instance;
}



class EnumTestEnumIntegerEnum {
  /// Instantiate a new enum with the provided [value].
  const EnumTestEnumIntegerEnum._(this.value);

  /// The underlying value of this enum member.
  final int value;

  @override
  String toString() => value.toString();

  int toJson() => value;

  static const number1 = EnumTestEnumIntegerEnum._(1);
  static const numberNegative1 = EnumTestEnumIntegerEnum._(-1);

  /// List of all possible values in this [enum][EnumTestEnumIntegerEnum].
  static const values = <EnumTestEnumIntegerEnum>[
    number1,
    numberNegative1,
  ];

  static EnumTestEnumIntegerEnum? fromJson(dynamic value) => EnumTestEnumIntegerEnumTypeTransformer().decode(value);

  static List<EnumTestEnumIntegerEnum>? listFromJson(dynamic json, {bool growable = false,}) {
    final result = <EnumTestEnumIntegerEnum>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = EnumTestEnumIntegerEnum.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }
}

/// Transformation class that can [encode] an instance of [EnumTestEnumIntegerEnum] to int,
/// and [decode] dynamic data back to [EnumTestEnumIntegerEnum].
class EnumTestEnumIntegerEnumTypeTransformer {
  factory EnumTestEnumIntegerEnumTypeTransformer() => _instance ??= const EnumTestEnumIntegerEnumTypeTransformer._();

  const EnumTestEnumIntegerEnumTypeTransformer._();

  int encode(EnumTestEnumIntegerEnum data) => data.value;

  /// Decodes a [dynamic value][data] to a EnumTestEnumIntegerEnum.
  ///
  /// If [allowNull] is true and the [dynamic value][data] cannot be decoded successfully,
  /// then null is returned. However, if [allowNull] is false and the [dynamic value][data]
  /// cannot be decoded successfully, then an [UnimplementedError] is thrown.
  ///
  /// The [allowNull] is very handy when an API changes and a new enum value is added or removed,
  /// and users are still using an old app with the old code.
  EnumTestEnumIntegerEnum? decode(dynamic data, {bool allowNull = true}) {
    if (data != null) {
      switch (data.toString()) {
        case 1: return EnumTestEnumIntegerEnum.number1;
        case -1: return EnumTestEnumIntegerEnum.numberNegative1;
        default:
          if (!allowNull) {
            throw ArgumentError('Unknown enum value to decode: $data');
          }
      }
    }
    return null;
  }

  /// Singleton [EnumTestEnumIntegerEnumTypeTransformer] instance.
  static EnumTestEnumIntegerEnumTypeTransformer? _instance;
}



class EnumTestEnumNumberEnum {
  /// Instantiate a new enum with the provided [value].
  const EnumTestEnumNumberEnum._(this.value);

  /// The underlying value of this enum member.
  final double value;

  @override
  String toString() => value.toString();

  double toJson() => value;

  static const number1Period1 = EnumTestEnumNumberEnum._('1.1');
  static const numberNegative1Period2 = EnumTestEnumNumberEnum._('-1.2');

  /// List of all possible values in this [enum][EnumTestEnumNumberEnum].
  static const values = <EnumTestEnumNumberEnum>[
    number1Period1,
    numberNegative1Period2,
  ];

  static EnumTestEnumNumberEnum? fromJson(dynamic value) => EnumTestEnumNumberEnumTypeTransformer().decode(value);

  static List<EnumTestEnumNumberEnum>? listFromJson(dynamic json, {bool growable = false,}) {
    final result = <EnumTestEnumNumberEnum>[];
    if (json is List && json.isNotEmpty) {
      for (final row in json) {
        final value = EnumTestEnumNumberEnum.fromJson(row);
        if (value != null) {
          result.add(value);
        }
      }
    }
    return result.toList(growable: growable);
  }
}

/// Transformation class that can [encode] an instance of [EnumTestEnumNumberEnum] to double,
/// and [decode] dynamic data back to [EnumTestEnumNumberEnum].
class EnumTestEnumNumberEnumTypeTransformer {
  factory EnumTestEnumNumberEnumTypeTransformer() => _instance ??= const EnumTestEnumNumberEnumTypeTransformer._();

  const EnumTestEnumNumberEnumTypeTransformer._();

  double encode(EnumTestEnumNumberEnum data) => data.value;

  /// Decodes a [dynamic value][data] to a EnumTestEnumNumberEnum.
  ///
  /// If [allowNull] is true and the [dynamic value][data] cannot be decoded successfully,
  /// then null is returned. However, if [allowNull] is false and the [dynamic value][data]
  /// cannot be decoded successfully, then an [UnimplementedError] is thrown.
  ///
  /// The [allowNull] is very handy when an API changes and a new enum value is added or removed,
  /// and users are still using an old app with the old code.
  EnumTestEnumNumberEnum? decode(dynamic data, {bool allowNull = true}) {
    if (data != null) {
      switch (data.toString()) {
        case '1.1': return EnumTestEnumNumberEnum.number1Period1;
        case '-1.2': return EnumTestEnumNumberEnum.numberNegative1Period2;
        default:
          if (!allowNull) {
            throw ArgumentError('Unknown enum value to decode: $data');
          }
      }
    }
    return null;
  }

  /// Singleton [EnumTestEnumNumberEnumTypeTransformer] instance.
  static EnumTestEnumNumberEnumTypeTransformer? _instance;
}


