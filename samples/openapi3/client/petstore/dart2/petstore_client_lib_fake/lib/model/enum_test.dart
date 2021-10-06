//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.14

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

  OuterEnumInteger? outerEnumInteger;

  OuterEnumDefaultValue? outerEnumDefaultValue;

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
    enumString.hashCode +
    enumStringRequired.hashCode +
    enumInteger.hashCode +
    enumNumber.hashCode +
    outerEnum.hashCode +
    outerEnumInteger.hashCode +
    outerEnumDefaultValue.hashCode +
    outerEnumIntegerDefaultValue.hashCode;

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
  static EnumTest fromJson(Map<String, dynamic> json) => EnumTest(
        enumString: EnumTestEnumStringEnum.fromJson(json[r'enum_string']),
        enumStringRequired: EnumTestEnumStringRequiredEnum.fromJson(json[r'enum_string_required']),
        enumInteger: EnumTestEnumIntegerEnum.fromJson(json[r'enum_integer']),
        enumNumber: EnumTestEnumNumberEnum.fromJson(json[r'enum_number']),
        outerEnum: OuterEnum.fromJson(json[r'outerEnum']),
        outerEnumInteger: OuterEnumInteger.fromJson(json[r'outerEnumInteger']),
        outerEnumDefaultValue: OuterEnumDefaultValue.fromJson(json[r'outerEnumDefaultValue']),
        outerEnumIntegerDefaultValue: OuterEnumIntegerDefaultValue.fromJson(json[r'outerEnumIntegerDefaultValue']),
    );

  static List<EnumTest> listFromJson(List json, {bool? growable,}) =>
    json.isNotEmpty
      ? json.map<EnumTest>((i) => EnumTest.fromJson(i as Map<String, dynamic>)).toList(growable: true == growable)
      : <EnumTest>[];

  static Map<String, EnumTest> mapFromJson(dynamic json) {
    final map = <String, EnumTest>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) => map[key] = EnumTest.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of EnumTest-objects as value to a dart map
  static Map<String, List<EnumTest>> mapListFromJson(dynamic json, {bool? growable,}) {
    final map = <String, List<EnumTest>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = EnumTest.listFromJson(
            value,
            growable: growable,
          );
        });
    }
    return map;
  }
}


class EnumTestEnumStringEnum {
  /// Instantiate a new enum with the provided [value].
  const EnumTestEnumStringEnum._(this.value);

  /// The underlying value of this enum member.
  final String? value;

  @override
  String toString() => value ?? '';

  String? toJson() => value;

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

  static List<EnumTestEnumStringEnum> listFromJson(List json, {bool? growable,}) =>
    json.isNotEmpty
      ? json.map<EnumTestEnumStringEnum>((i) => EnumTestEnumStringEnum.fromJson(i as Map<String, dynamic>)).toList(growable: true == growable)
      : <EnumTestEnumStringEnum>[];
}

/// Transformation class that can [encode] an instance of [EnumTestEnumStringEnum] to String,
/// and [decode] dynamic data back to [EnumTestEnumStringEnum].
class EnumTestEnumStringEnumTypeTransformer {
  factory EnumTestEnumStringEnumTypeTransformer() => _instance ??= const EnumTestEnumStringEnumTypeTransformer._();

  const EnumTestEnumStringEnumTypeTransformer._();

  String? encode(EnumTestEnumStringEnum data) => data.value;

  /// Decodes a [dynamic value][data] to a EnumTestEnumStringEnum.
  ///
  /// If the [dynamic value][data] cannot be decoded successfully, then an [UnimplementedError] is thrown.
  EnumTestEnumStringEnum decode(dynamic data) {
    if (data == r'UPPER') {
      return EnumTestEnumStringEnum.UPPER;
    }
    if (data == r'lower') {
      return EnumTestEnumStringEnum.lower;
    }
    if (data == r'') {
      return EnumTestEnumStringEnum.empty;
    }
    throw ArgumentError('Unknown enum value to decode: $data');
  }

  /// Singleton [EnumTestEnumStringEnumTypeTransformer] instance.
  static EnumTestEnumStringEnumTypeTransformer? _instance;
}



class EnumTestEnumStringRequiredEnum {
  /// Instantiate a new enum with the provided [value].
  const EnumTestEnumStringRequiredEnum._(this.value);

  /// The underlying value of this enum member.
  final String? value;

  @override
  String toString() => value ?? '';

  String? toJson() => value;

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

  static List<EnumTestEnumStringRequiredEnum> listFromJson(List json, {bool? growable,}) =>
    json.isNotEmpty
      ? json.map<EnumTestEnumStringRequiredEnum>((i) => EnumTestEnumStringRequiredEnum.fromJson(i as Map<String, dynamic>)).toList(growable: true == growable)
      : <EnumTestEnumStringRequiredEnum>[];
}

/// Transformation class that can [encode] an instance of [EnumTestEnumStringRequiredEnum] to String,
/// and [decode] dynamic data back to [EnumTestEnumStringRequiredEnum].
class EnumTestEnumStringRequiredEnumTypeTransformer {
  factory EnumTestEnumStringRequiredEnumTypeTransformer() => _instance ??= const EnumTestEnumStringRequiredEnumTypeTransformer._();

  const EnumTestEnumStringRequiredEnumTypeTransformer._();

  String? encode(EnumTestEnumStringRequiredEnum data) => data.value;

  /// Decodes a [dynamic value][data] to a EnumTestEnumStringRequiredEnum.
  ///
  /// If the [dynamic value][data] cannot be decoded successfully, then an [UnimplementedError] is thrown.
  EnumTestEnumStringRequiredEnum decode(dynamic data) {
    if (data == r'UPPER') {
      return EnumTestEnumStringRequiredEnum.UPPER;
    }
    if (data == r'lower') {
      return EnumTestEnumStringRequiredEnum.lower;
    }
    if (data == r'') {
      return EnumTestEnumStringRequiredEnum.empty;
    }
    throw ArgumentError('Unknown enum value to decode: $data');
  }

  /// Singleton [EnumTestEnumStringRequiredEnumTypeTransformer] instance.
  static EnumTestEnumStringRequiredEnumTypeTransformer? _instance;
}



class EnumTestEnumIntegerEnum {
  /// Instantiate a new enum with the provided [value].
  const EnumTestEnumIntegerEnum._(this.value);

  /// The underlying value of this enum member.
  final int? value;

  @override
  String toString() => value == null ? '' : value.toString();

  int? toJson() => value;

  static const number1 = EnumTestEnumIntegerEnum._(1);
  static const numberNegative1 = EnumTestEnumIntegerEnum._(-1);

  /// List of all possible values in this [enum][EnumTestEnumIntegerEnum].
  static const values = <EnumTestEnumIntegerEnum>[
    number1,
    numberNegative1,
  ];

  static EnumTestEnumIntegerEnum fromJson(dynamic value) =>
    EnumTestEnumIntegerEnumTypeTransformer().decode(value);

  static List<EnumTestEnumIntegerEnum> listFromJson(List json, {bool? growable,}) =>
    json.isNotEmpty
      ? json.map<EnumTestEnumIntegerEnum>((i) => EnumTestEnumIntegerEnum.fromJson(i as Map<String, dynamic>)).toList(growable: true == growable)
      : <EnumTestEnumIntegerEnum>[];
}

/// Transformation class that can [encode] an instance of [EnumTestEnumIntegerEnum] to int,
/// and [decode] dynamic data back to [EnumTestEnumIntegerEnum].
class EnumTestEnumIntegerEnumTypeTransformer {
  factory EnumTestEnumIntegerEnumTypeTransformer() => _instance ??= const EnumTestEnumIntegerEnumTypeTransformer._();

  const EnumTestEnumIntegerEnumTypeTransformer._();

  int? encode(EnumTestEnumIntegerEnum data) => data.value;

  /// Decodes a [dynamic value][data] to a EnumTestEnumIntegerEnum.
  ///
  /// If the [dynamic value][data] cannot be decoded successfully, then an [UnimplementedError] is thrown.
  EnumTestEnumIntegerEnum decode(dynamic data) {
    if (data == 1) {
      return EnumTestEnumIntegerEnum.number1;
    }
    if (data == -1) {
      return EnumTestEnumIntegerEnum.numberNegative1;
    }
    throw ArgumentError('Unknown enum value to decode: $data');
  }

  /// Singleton [EnumTestEnumIntegerEnumTypeTransformer] instance.
  static EnumTestEnumIntegerEnumTypeTransformer? _instance;
}



class EnumTestEnumNumberEnum {
  /// Instantiate a new enum with the provided [value].
  const EnumTestEnumNumberEnum._(this.value);

  /// The underlying value of this enum member.
  final double? value;

  @override
  String toString() => value == null ? '' : value.toString();

  double? toJson() => value;

  static const number1Period1 = EnumTestEnumNumberEnum._(1.1);
  static const numberNegative1Period2 = EnumTestEnumNumberEnum._(-1.2);

  /// List of all possible values in this [enum][EnumTestEnumNumberEnum].
  static const values = <EnumTestEnumNumberEnum>[
    number1Period1,
    numberNegative1Period2,
  ];

  static EnumTestEnumNumberEnum fromJson(dynamic value) =>
    EnumTestEnumNumberEnumTypeTransformer().decode(value);

  static List<EnumTestEnumNumberEnum> listFromJson(List json, {bool? growable,}) =>
    json.isNotEmpty
      ? json.map<EnumTestEnumNumberEnum>((i) => EnumTestEnumNumberEnum.fromJson(i as Map<String, dynamic>)).toList(growable: true == growable)
      : <EnumTestEnumNumberEnum>[];
}

/// Transformation class that can [encode] an instance of [EnumTestEnumNumberEnum] to double,
/// and [decode] dynamic data back to [EnumTestEnumNumberEnum].
class EnumTestEnumNumberEnumTypeTransformer {
  factory EnumTestEnumNumberEnumTypeTransformer() => _instance ??= const EnumTestEnumNumberEnumTypeTransformer._();

  const EnumTestEnumNumberEnumTypeTransformer._();

  double? encode(EnumTestEnumNumberEnum data) => data.value;

  /// Decodes a [dynamic value][data] to a EnumTestEnumNumberEnum.
  ///
  /// If the [dynamic value][data] cannot be decoded successfully, then an [UnimplementedError] is thrown.
  EnumTestEnumNumberEnum decode(dynamic data) {
    if (data == 1.1) {
      return EnumTestEnumNumberEnum.number1Period1;
    }
    if (data == -1.2) {
      return EnumTestEnumNumberEnum.numberNegative1Period2;
    }
    throw ArgumentError('Unknown enum value to decode: $data');
  }

  /// Singleton [EnumTestEnumNumberEnumTypeTransformer] instance.
  static EnumTestEnumNumberEnumTypeTransformer? _instance;
}


