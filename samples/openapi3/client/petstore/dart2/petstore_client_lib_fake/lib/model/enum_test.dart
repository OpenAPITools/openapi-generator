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
      json[r'enum_string'] = enumString;
    }
    if (enumStringRequired != null) {
      json[r'enum_string_required'] = enumStringRequired;
    }
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
  static EnumTest fromJson(Map<String, dynamic> json) => json == null
    ? null
    : EnumTest(
        enumString: _$enumDecode(_$EnumTestEnumStringEnum, json[r'enum_string']),
        enumStringRequired: _$enumDecode(_$EnumTestEnumStringRequiredEnum, json[r'enum_string_required']),
        enumInteger: _$enumDecode(_$EnumTestEnumIntegerEnum, json[r'enum_integer']),
        enumNumber: _$enumDecode(_$EnumTestEnumNumberEnum, json[r'enum_number']),
        not enum
        outerEnum: OuterEnum.fromJson(json[r'outerEnum']),
        not enum
        outerEnumInteger: OuterEnumInteger.fromJson(json[r'outerEnumInteger']),
        not enum
        outerEnumDefaultValue: OuterEnumDefaultValue.fromJson(json[r'outerEnumDefaultValue']),
        not enum
        outerEnumIntegerDefaultValue: OuterEnumIntegerDefaultValue.fromJson(json[r'outerEnumIntegerDefaultValue']),
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


enum EnumTestEnumStringEnum {
        UPPER,
        lower,
        empty,
}

const _$EnumTestEnumStringEnum = <EnumTestEnumStringEnum, dynamic>{
        EnumTestEnumStringEnum.UPPER: 'UPPER',
        EnumTestEnumStringEnum.lower: 'lower',
        EnumTestEnumStringEnum.empty: '',
};



enum EnumTestEnumStringRequiredEnum {
        UPPER,
        lower,
        empty,
}

const _$EnumTestEnumStringRequiredEnum = <EnumTestEnumStringRequiredEnum, dynamic>{
        EnumTestEnumStringRequiredEnum.UPPER: 'UPPER',
        EnumTestEnumStringRequiredEnum.lower: 'lower',
        EnumTestEnumStringRequiredEnum.empty: '',
};



enum EnumTestEnumIntegerEnum {
        number1,
        numberNegative1,
}

const _$EnumTestEnumIntegerEnum = <EnumTestEnumIntegerEnum, dynamic>{
        EnumTestEnumIntegerEnum.number1: 1,
        EnumTestEnumIntegerEnum.numberNegative1: -1,
};



enum EnumTestEnumNumberEnum {
        number1Period1,
        numberNegative1Period2,
}

const _$EnumTestEnumNumberEnum = <EnumTestEnumNumberEnum, dynamic>{
        EnumTestEnumNumberEnum.number1Period1: '1.1',
        EnumTestEnumNumberEnum.numberNegative1Period2: '-1.2',
};


