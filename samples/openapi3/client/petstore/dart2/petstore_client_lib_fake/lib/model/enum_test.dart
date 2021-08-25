//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class EnumTest {
  /// Returns a new [EnumTest] instance.
  EnumTest({
    this.enumString,
    @required this.enumStringRequired,
    this.enumInteger,
    this.enumNumber,
    this.outerEnum,
    this.outerEnumInteger,
    this.outerEnumDefaultValue,
    this.outerEnumIntegerDefaultValue,
  });

  EnumTestEnumStringEnum enumString;

  EnumTestEnumStringRequiredEnum enumStringRequired;

  EnumTestEnumIntegerEnum enumInteger;

  EnumTestEnumNumberEnum enumNumber;

  OuterEnum outerEnum;

  OuterEnumInteger outerEnumInteger;

  OuterEnumDefaultValue outerEnumDefaultValue;

  OuterEnumIntegerDefaultValue outerEnumIntegerDefaultValue;

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
    (enumString == null ? 0 : enumString.hashCode) +
    (enumStringRequired == null ? 0 : enumStringRequired.hashCode) +
    (enumInteger == null ? 0 : enumInteger.hashCode) +
    (enumNumber == null ? 0 : enumNumber.hashCode) +
    (outerEnum == null ? 0 : outerEnum.hashCode) +
    (outerEnumInteger == null ? 0 : outerEnumInteger.hashCode) +
    (outerEnumDefaultValue == null ? 0 : outerEnumDefaultValue.hashCode) +
    (outerEnumIntegerDefaultValue == null ? 0 : outerEnumIntegerDefaultValue.hashCode);

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
  /// [json] if it's non-null, null if [json] is null.
  // ignore: prefer_constructors_over_static_methods
  static EnumTest fromJson(Map<String, dynamic> json) => json == null
    ? null
    : EnumTest(
        enumString: json[r'enum_string'] is Map
          ? EnumTestEnumStringEnum.fromJson((json[r'enum_string'] as Map).cast<String, dynamic>())
          : null,
        enumStringRequired: json[r'enum_string_required'] is Map
          ? EnumTestEnumStringRequiredEnum.fromJson((json[r'enum_string_required'] as Map).cast<String, dynamic>())
          : null,
        enumInteger: json[r'enum_integer'] is Map
          ? EnumTestEnumIntegerEnum.fromJson((json[r'enum_integer'] as Map).cast<String, dynamic>())
          : null,
        enumNumber: json[r'enum_number'] is Map
          ? EnumTestEnumNumberEnum.fromJson((json[r'enum_number'] as Map).cast<String, dynamic>())
          : null,
        outerEnum: json[r'outerEnum'] is Map
          ? OuterEnum.fromJson((json[r'outerEnum'] as Map).cast<String, dynamic>())
          : null,
        outerEnumInteger: json[r'outerEnumInteger'] is Map
          ? OuterEnumInteger.fromJson((json[r'outerEnumInteger'] as Map).cast<String, dynamic>())
          : null,
        outerEnumDefaultValue: json[r'outerEnumDefaultValue'] is Map
          ? OuterEnumDefaultValue.fromJson((json[r'outerEnumDefaultValue'] as Map).cast<String, dynamic>())
          : null,
        outerEnumIntegerDefaultValue: json[r'outerEnumIntegerDefaultValue'] is Map
          ? OuterEnumIntegerDefaultValue.fromJson((json[r'outerEnumIntegerDefaultValue'] as Map).cast<String, dynamic>())
          : null,
    );

  static List<EnumTest> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <EnumTest>[]
      : json
          .map((dynamic value) => EnumTest.fromJson((value as Map).cast<String, dynamic>()))
          .toList(growable: true == growable);

  static Map<String, EnumTest> mapFromJson(Map<String, dynamic> json) {
    final map = <String, EnumTest>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, dynamic value) => map[key] = EnumTest.fromJson((value as Map).cast<String, dynamic>()));
    }
    return map;
  }

  // maps a json object with a list of EnumTest-objects as value to a dart map
  static Map<String, List<EnumTest>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<EnumTest>>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, dynamic value) {
        map[key] = EnumTest.listFromJson(value as List, emptyIsNull: emptyIsNull, growable: growable,);
      });
    }
    return map;
  }
}



// ignore_for_file: constant_identifier_names

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

  static EnumTestEnumStringEnum fromJson(dynamic value) =>
    EnumTestEnumStringEnumTypeTransformer().decode(value);

  static List<EnumTestEnumStringEnum> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <EnumTestEnumStringEnum>[]
      : json.map(EnumTestEnumStringEnum.fromJson).toList(growable: true == growable);
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
  EnumTestEnumStringEnum decode(dynamic data, {bool allowNull}) {
    switch ('$data') {
      case r'UPPER': return EnumTestEnumStringEnum.UPPER;
      case r'lower': return EnumTestEnumStringEnum.lower;
      case r'': return EnumTestEnumStringEnum.empty;
      default:
        if (allowNull == false) {
          throw ArgumentError('Unknown enum value to decode: $data');
        }
    }
    return null;
  }

  /// Singleton [EnumTestEnumStringEnumTypeTransformer] instance.
  static EnumTestEnumStringEnumTypeTransformer _instance;
}



// ignore_for_file: constant_identifier_names

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

  static EnumTestEnumStringRequiredEnum fromJson(dynamic value) =>
    EnumTestEnumStringRequiredEnumTypeTransformer().decode(value);

  static List<EnumTestEnumStringRequiredEnum> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <EnumTestEnumStringRequiredEnum>[]
      : json.map(EnumTestEnumStringRequiredEnum.fromJson).toList(growable: true == growable);
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
  EnumTestEnumStringRequiredEnum decode(dynamic data, {bool allowNull}) {
    switch ('$data') {
      case r'UPPER': return EnumTestEnumStringRequiredEnum.UPPER;
      case r'lower': return EnumTestEnumStringRequiredEnum.lower;
      case r'': return EnumTestEnumStringRequiredEnum.empty;
      default:
        if (allowNull == false) {
          throw ArgumentError('Unknown enum value to decode: $data');
        }
    }
    return null;
  }

  /// Singleton [EnumTestEnumStringRequiredEnumTypeTransformer] instance.
  static EnumTestEnumStringRequiredEnumTypeTransformer _instance;
}



// ignore_for_file: constant_identifier_names

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

  static EnumTestEnumIntegerEnum fromJson(dynamic value) =>
    EnumTestEnumIntegerEnumTypeTransformer().decode(value);

  static List<EnumTestEnumIntegerEnum> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <EnumTestEnumIntegerEnum>[]
      : json.map(EnumTestEnumIntegerEnum.fromJson).toList(growable: true == growable);
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
  EnumTestEnumIntegerEnum decode(dynamic data, {bool allowNull}) {
    switch ('$data') {
      case 1: return EnumTestEnumIntegerEnum.number1;
      case -1: return EnumTestEnumIntegerEnum.numberNegative1;
      default:
        if (allowNull == false) {
          throw ArgumentError('Unknown enum value to decode: $data');
        }
    }
    return null;
  }

  /// Singleton [EnumTestEnumIntegerEnumTypeTransformer] instance.
  static EnumTestEnumIntegerEnumTypeTransformer _instance;
}



// ignore_for_file: constant_identifier_names

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

  static EnumTestEnumNumberEnum fromJson(dynamic value) =>
    EnumTestEnumNumberEnumTypeTransformer().decode(value);

  static List<EnumTestEnumNumberEnum> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <EnumTestEnumNumberEnum>[]
      : json.map(EnumTestEnumNumberEnum.fromJson).toList(growable: true == growable);
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
  EnumTestEnumNumberEnum decode(dynamic data, {bool allowNull}) {
    switch ('$data') {
      case '1.1': return EnumTestEnumNumberEnum.number1Period1;
      case '-1.2': return EnumTestEnumNumberEnum.numberNegative1Period2;
      default:
        if (allowNull == false) {
          throw ArgumentError('Unknown enum value to decode: $data');
        }
    }
    return null;
  }

  /// Singleton [EnumTestEnumNumberEnumTypeTransformer] instance.
  static EnumTestEnumNumberEnumTypeTransformer _instance;
}

