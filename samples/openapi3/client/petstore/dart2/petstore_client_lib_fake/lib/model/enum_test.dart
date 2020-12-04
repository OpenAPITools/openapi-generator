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
      json['enum_string'] = enumString;
    }
    if (enumStringRequired != null) {
      json['enum_string_required'] = enumStringRequired;
    }
    if (enumInteger != null) {
      json['enum_integer'] = enumInteger;
    }
    if (enumNumber != null) {
      json['enum_number'] = enumNumber;
    }
    if (outerEnum != null) {
      json['outerEnum'] = outerEnum;
    }
    if (outerEnumInteger != null) {
      json['outerEnumInteger'] = outerEnumInteger;
    }
    if (outerEnumDefaultValue != null) {
      json['outerEnumDefaultValue'] = outerEnumDefaultValue;
    }
    if (outerEnumIntegerDefaultValue != null) {
      json['outerEnumIntegerDefaultValue'] = outerEnumIntegerDefaultValue;
    }
    return json;
  }

  /// Returns a new [EnumTest] instance and imports its values from
  /// [json] if it's non-null, null if [json] is null.
  static EnumTest fromJson(Map<String, dynamic> json) => json == null
    ? null
    : EnumTest(
        enumString: EnumTestEnumStringEnum.fromJson(json['enum_string']),
        enumStringRequired: EnumTestEnumStringRequiredEnum.fromJson(json['enum_string_required']),
        enumInteger: EnumTestEnumIntegerEnum.fromJson(json['enum_integer']),
        enumNumber: EnumTestEnumNumberEnum.fromJson(json['enum_number']),
        outerEnum: OuterEnum.fromJson(json['outerEnum']),
        outerEnumInteger: OuterEnumInteger.fromJson(json['outerEnumInteger']),
        outerEnumDefaultValue: OuterEnumDefaultValue.fromJson(json['outerEnumDefaultValue']),
        outerEnumIntegerDefaultValue: OuterEnumIntegerDefaultValue.fromJson(json['outerEnumIntegerDefaultValue']),
    );

  static List<EnumTest> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <EnumTest>[]
      : json.map((v) => EnumTest.fromJson(v)).toList(growable: true == growable);

  static Map<String, EnumTest> mapFromJson(Map<String, dynamic> json) {
    final map = <String, EnumTest>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) => map[key] = EnumTest.fromJson(v));
    }
    return map;
  }

  // maps a json object with a list of EnumTest-objects as value to a dart map
  static Map<String, List<EnumTest>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<EnumTest>>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) {
        map[key] = EnumTest.listFromJson(v, emptyIsNull: emptyIsNull, growable: growable);
      });
    }
    return map;
  }
}


class EnumTestEnumStringEnum {
  /// Instantiate a new enum with the provided [value].
  const EnumTestEnumStringEnum._(this.value);

  /// The underlying value of this enum member.
  final String value;

  @override
  bool operator ==(Object other) => identical(this, other) ||
      other is EnumTestEnumStringEnum && other.value == value ||
      other is String && other == value;

  @override
  int get hashCode => toString().hashCode;

  @override
  String toString() => value;

  String toJson() => value;

  static const upper_ = EnumTestEnumStringEnum._('UPPER');
  static const lower_ = EnumTestEnumStringEnum._('lower');
  static const empty = EnumTestEnumStringEnum._('');

  /// List of all possible values in this [enum][EnumTestEnumStringEnum].
  static const values = <EnumTestEnumStringEnum>[
    upper_,
    lower_,
    empty,
  ];

  static EnumTestEnumStringEnum fromJson(dynamic value) =>
    EnumTestEnumStringEnumTypeTransformer().decode(value);

  static List<EnumTestEnumStringEnum> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <EnumTestEnumStringEnum>[]
      : json
          .map((value) => EnumTestEnumStringEnum.fromJson(value))
          .toList(growable: true == growable);
}

/// Transformation class that can [encode] an instance of [EnumTestEnumStringEnum] to String,
/// and [decode] dynamic data back to [EnumTestEnumStringEnum].
class EnumTestEnumStringEnumTypeTransformer {
  const EnumTestEnumStringEnumTypeTransformer._();

  factory EnumTestEnumStringEnumTypeTransformer() => _instance ??= EnumTestEnumStringEnumTypeTransformer._();

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
    switch (data) {
      case 'UPPER': return EnumTestEnumStringEnum.upper_;
      case 'lower': return EnumTestEnumStringEnum.lower_;
      case '': return EnumTestEnumStringEnum.empty;
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


class EnumTestEnumStringRequiredEnum {
  /// Instantiate a new enum with the provided [value].
  const EnumTestEnumStringRequiredEnum._(this.value);

  /// The underlying value of this enum member.
  final String value;

  @override
  bool operator ==(Object other) => identical(this, other) ||
      other is EnumTestEnumStringRequiredEnum && other.value == value ||
      other is String && other == value;

  @override
  int get hashCode => toString().hashCode;

  @override
  String toString() => value;

  String toJson() => value;

  static const upper_ = EnumTestEnumStringRequiredEnum._('UPPER');
  static const lower_ = EnumTestEnumStringRequiredEnum._('lower');
  static const empty = EnumTestEnumStringRequiredEnum._('');

  /// List of all possible values in this [enum][EnumTestEnumStringRequiredEnum].
  static const values = <EnumTestEnumStringRequiredEnum>[
    upper_,
    lower_,
    empty,
  ];

  static EnumTestEnumStringRequiredEnum fromJson(dynamic value) =>
    EnumTestEnumStringRequiredEnumTypeTransformer().decode(value);

  static List<EnumTestEnumStringRequiredEnum> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <EnumTestEnumStringRequiredEnum>[]
      : json
          .map((value) => EnumTestEnumStringRequiredEnum.fromJson(value))
          .toList(growable: true == growable);
}

/// Transformation class that can [encode] an instance of [EnumTestEnumStringRequiredEnum] to String,
/// and [decode] dynamic data back to [EnumTestEnumStringRequiredEnum].
class EnumTestEnumStringRequiredEnumTypeTransformer {
  const EnumTestEnumStringRequiredEnumTypeTransformer._();

  factory EnumTestEnumStringRequiredEnumTypeTransformer() => _instance ??= EnumTestEnumStringRequiredEnumTypeTransformer._();

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
    switch (data) {
      case 'UPPER': return EnumTestEnumStringRequiredEnum.upper_;
      case 'lower': return EnumTestEnumStringRequiredEnum.lower_;
      case '': return EnumTestEnumStringRequiredEnum.empty;
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


class EnumTestEnumIntegerEnum {
  /// Instantiate a new enum with the provided [value].
  const EnumTestEnumIntegerEnum._(this.value);

  /// The underlying value of this enum member.
  final String value;

  @override
  bool operator ==(Object other) => identical(this, other) ||
      other is EnumTestEnumIntegerEnum && other.value == value ||
      other is String && other == value;

  @override
  int get hashCode => toString().hashCode;

  @override
  String toString() => value.toString();

  String toJson() => value;

  static const number1_ = EnumTestEnumIntegerEnum._(1);
  static const number1_ = EnumTestEnumIntegerEnum._(-1);

  /// List of all possible values in this [enum][EnumTestEnumIntegerEnum].
  static const values = <EnumTestEnumIntegerEnum>[
    number1_,
    number1_,
  ];

  static EnumTestEnumIntegerEnum fromJson(dynamic value) =>
    EnumTestEnumIntegerEnumTypeTransformer().decode(value);

  static List<EnumTestEnumIntegerEnum> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <EnumTestEnumIntegerEnum>[]
      : json
          .map((value) => EnumTestEnumIntegerEnum.fromJson(value))
          .toList(growable: true == growable);
}

/// Transformation class that can [encode] an instance of [EnumTestEnumIntegerEnum] to int,
/// and [decode] dynamic data back to [EnumTestEnumIntegerEnum].
class EnumTestEnumIntegerEnumTypeTransformer {
  const EnumTestEnumIntegerEnumTypeTransformer._();

  factory EnumTestEnumIntegerEnumTypeTransformer() => _instance ??= EnumTestEnumIntegerEnumTypeTransformer._();

  String encode(EnumTestEnumIntegerEnum data) => data.value;

  /// Decodes a [dynamic value][data] to a EnumTestEnumIntegerEnum.
  ///
  /// If [allowNull] is true and the [dynamic value][data] cannot be decoded successfully,
  /// then null is returned. However, if [allowNull] is false and the [dynamic value][data]
  /// cannot be decoded successfully, then an [UnimplementedError] is thrown.
  ///
  /// The [allowNull] is very handy when an API changes and a new enum value is added or removed,
  /// and users are still using an old app with the old code.
  EnumTestEnumIntegerEnum decode(dynamic data, {bool allowNull}) {
    switch (data) {
      case 1: return EnumTestEnumIntegerEnum.number1_;
      case -1: return EnumTestEnumIntegerEnum.number1_;
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


class EnumTestEnumNumberEnum {
  /// Instantiate a new enum with the provided [value].
  const EnumTestEnumNumberEnum._(this.value);

  /// The underlying value of this enum member.
  final String value;

  @override
  bool operator ==(Object other) => identical(this, other) ||
      other is EnumTestEnumNumberEnum && other.value == value ||
      other is String && other == value;

  @override
  int get hashCode => toString().hashCode;

  @override
  String toString() => value.toString();

  String toJson() => value;

  static const 11_ = EnumTestEnumNumberEnum._('1.1');
  static const 12_ = EnumTestEnumNumberEnum._('-1.2');

  /// List of all possible values in this [enum][EnumTestEnumNumberEnum].
  static const values = <EnumTestEnumNumberEnum>[
    11_,
    12_,
  ];

  static EnumTestEnumNumberEnum fromJson(dynamic value) =>
    EnumTestEnumNumberEnumTypeTransformer().decode(value);

  static List<EnumTestEnumNumberEnum> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <EnumTestEnumNumberEnum>[]
      : json
          .map((value) => EnumTestEnumNumberEnum.fromJson(value))
          .toList(growable: true == growable);
}

/// Transformation class that can [encode] an instance of [EnumTestEnumNumberEnum] to double,
/// and [decode] dynamic data back to [EnumTestEnumNumberEnum].
class EnumTestEnumNumberEnumTypeTransformer {
  const EnumTestEnumNumberEnumTypeTransformer._();

  factory EnumTestEnumNumberEnumTypeTransformer() => _instance ??= EnumTestEnumNumberEnumTypeTransformer._();

  String encode(EnumTestEnumNumberEnum data) => data.value;

  /// Decodes a [dynamic value][data] to a EnumTestEnumNumberEnum.
  ///
  /// If [allowNull] is true and the [dynamic value][data] cannot be decoded successfully,
  /// then null is returned. However, if [allowNull] is false and the [dynamic value][data]
  /// cannot be decoded successfully, then an [UnimplementedError] is thrown.
  ///
  /// The [allowNull] is very handy when an API changes and a new enum value is added or removed,
  /// and users are still using an old app with the old code.
  EnumTestEnumNumberEnum decode(dynamic data, {bool allowNull}) {
    switch (data) {
      case '1.1': return EnumTestEnumNumberEnum.11_;
      case '-1.2': return EnumTestEnumNumberEnum.12_;
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

