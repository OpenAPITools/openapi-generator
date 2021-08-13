//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class EnumArrays {
  /// Returns a new [EnumArrays] instance.
  EnumArrays({
    this.justSymbol,
    this.arrayEnum = const [],
  });

  EnumArraysJustSymbolEnum justSymbol;

  List<EnumArraysArrayEnumEnum> arrayEnum;

  @override
  bool operator ==(Object other) => identical(this, other) || other is EnumArrays &&
     other.justSymbol == justSymbol &&
     other.arrayEnum == arrayEnum;

  @override
  int get hashCode =>
    (justSymbol == null ? 0 : justSymbol.hashCode) +
    (arrayEnum == null ? 0 : arrayEnum.hashCode);

  @override
  String toString() => 'EnumArrays[justSymbol=$justSymbol, arrayEnum=$arrayEnum]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (justSymbol != null) {
      json[r'just_symbol'] = justSymbol;
    }
    if (arrayEnum != null) {
      json[r'array_enum'] = arrayEnum;
    }
    return json;
  }

  /// Returns a new [EnumArrays] instance and imports its values from
  /// [json] if it's non-null, null if [json] is null.
  static EnumArrays fromJson(Map<String, dynamic> json) => json == null
    ? null
    : EnumArrays(
        justSymbol: EnumArraysJustSymbolEnum.fromJson(json[r'just_symbol']),
        arrayEnum: EnumArraysArrayEnumEnum.listFromJson(json[r'array_enum']),
    );

  static List<EnumArrays> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <EnumArrays>[]
      : json.map((dynamic value) => EnumArrays.fromJson(value)).toList(growable: true == growable);

  static Map<String, EnumArrays> mapFromJson(Map<String, dynamic> json) {
    final map = <String, EnumArrays>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) => map[key] = EnumArrays.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of EnumArrays-objects as value to a dart map
  static Map<String, List<EnumArrays>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<EnumArrays>>{};
    if (json?.isNotEmpty == true) {
      json.forEach((key, value) {
        map[key] = EnumArrays.listFromJson(value, emptyIsNull: emptyIsNull, growable: growable,);
      });
    }
    return map;
  }
}


class EnumArraysJustSymbolEnum {
  /// Instantiate a new enum with the provided [value].
  const EnumArraysJustSymbolEnum._(this.value);

  /// The underlying value of this enum member.
  final String value;

  @override
  String toString() => value;

  String toJson() => value;

  static const greaterThanEqual = EnumArraysJustSymbolEnum._(r'>=');
  static const dollar = EnumArraysJustSymbolEnum._(r'$');

  /// List of all possible values in this [enum][EnumArraysJustSymbolEnum].
  static const values = <EnumArraysJustSymbolEnum>[
    greaterThanEqual,
    dollar,
  ];

  static EnumArraysJustSymbolEnum fromJson(dynamic value) =>
    EnumArraysJustSymbolEnumTypeTransformer().decode(value);

  static List<EnumArraysJustSymbolEnum> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <EnumArraysJustSymbolEnum>[]
      : json
          .map((value) => EnumArraysJustSymbolEnum.fromJson(value))
          .toList(growable: true == growable);
}

/// Transformation class that can [encode] an instance of [EnumArraysJustSymbolEnum] to String,
/// and [decode] dynamic data back to [EnumArraysJustSymbolEnum].
class EnumArraysJustSymbolEnumTypeTransformer {
  const EnumArraysJustSymbolEnumTypeTransformer._();

  factory EnumArraysJustSymbolEnumTypeTransformer() => _instance ??= EnumArraysJustSymbolEnumTypeTransformer._();

  String encode(EnumArraysJustSymbolEnum data) => data.value;

  /// Decodes a [dynamic value][data] to a EnumArraysJustSymbolEnum.
  ///
  /// If [allowNull] is true and the [dynamic value][data] cannot be decoded successfully,
  /// then null is returned. However, if [allowNull] is false and the [dynamic value][data]
  /// cannot be decoded successfully, then an [UnimplementedError] is thrown.
  ///
  /// The [allowNull] is very handy when an API changes and a new enum value is added or removed,
  /// and users are still using an old app with the old code.
  EnumArraysJustSymbolEnum decode(dynamic data, {bool allowNull}) {
    switch (data) {
      case r'>=': return EnumArraysJustSymbolEnum.greaterThanEqual;
      case r'$': return EnumArraysJustSymbolEnum.dollar;
      default:
        if (allowNull == false) {
          throw ArgumentError('Unknown enum value to decode: $data');
        }
    }
    return null;
  }

  /// Singleton [EnumArraysJustSymbolEnumTypeTransformer] instance.
  static EnumArraysJustSymbolEnumTypeTransformer _instance;
}


class EnumArraysArrayEnumEnum {
  /// Instantiate a new enum with the provided [value].
  const EnumArraysArrayEnumEnum._(this.value);

  /// The underlying value of this enum member.
  final String value;

  @override
  String toString() => value;

  String toJson() => value;

  static const fish = EnumArraysArrayEnumEnum._(r'fish');
  static const crab = EnumArraysArrayEnumEnum._(r'crab');

  /// List of all possible values in this [enum][EnumArraysArrayEnumEnum].
  static const values = <EnumArraysArrayEnumEnum>[
    fish,
    crab,
  ];

  static EnumArraysArrayEnumEnum fromJson(dynamic value) =>
    EnumArraysArrayEnumEnumTypeTransformer().decode(value);

  static List<EnumArraysArrayEnumEnum> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <EnumArraysArrayEnumEnum>[]
      : json
          .map((value) => EnumArraysArrayEnumEnum.fromJson(value))
          .toList(growable: true == growable);
}

/// Transformation class that can [encode] an instance of [EnumArraysArrayEnumEnum] to String,
/// and [decode] dynamic data back to [EnumArraysArrayEnumEnum].
class EnumArraysArrayEnumEnumTypeTransformer {
  const EnumArraysArrayEnumEnumTypeTransformer._();

  factory EnumArraysArrayEnumEnumTypeTransformer() => _instance ??= EnumArraysArrayEnumEnumTypeTransformer._();

  String encode(EnumArraysArrayEnumEnum data) => data.value;

  /// Decodes a [dynamic value][data] to a EnumArraysArrayEnumEnum.
  ///
  /// If [allowNull] is true and the [dynamic value][data] cannot be decoded successfully,
  /// then null is returned. However, if [allowNull] is false and the [dynamic value][data]
  /// cannot be decoded successfully, then an [UnimplementedError] is thrown.
  ///
  /// The [allowNull] is very handy when an API changes and a new enum value is added or removed,
  /// and users are still using an old app with the old code.
  EnumArraysArrayEnumEnum decode(dynamic data, {bool allowNull}) {
    switch (data) {
      case r'fish': return EnumArraysArrayEnumEnum.fish;
      case r'crab': return EnumArraysArrayEnumEnum.crab;
      default:
        if (allowNull == false) {
          throw ArgumentError('Unknown enum value to decode: $data');
        }
    }
    return null;
  }

  /// Singleton [EnumArraysArrayEnumEnumTypeTransformer] instance.
  static EnumArraysArrayEnumEnumTypeTransformer _instance;
}

