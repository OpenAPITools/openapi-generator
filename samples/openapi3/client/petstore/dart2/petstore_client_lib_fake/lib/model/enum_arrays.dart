//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.14

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class EnumArrays {
  /// Returns a new [EnumArrays] instance.
  EnumArrays({
    this.justSymbol,
    this.arrayEnum = const [],
  });


  EnumArraysJustSymbolEnum? justSymbol;

  List<EnumArraysArrayEnumEnum>? arrayEnum;

  @override
  bool operator ==(Object other) => identical(this, other) || other is EnumArrays &&
     other.justSymbol == justSymbol &&
     other.arrayEnum == arrayEnum;

  @override
  int get hashCode =>
    justSymbol.hashCode +
    arrayEnum.hashCode;

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
  /// [value] if it's a [Map], null otherwise.
  // ignore: prefer_constructors_over_static_methods
  static EnumArrays fromJson(Map<String, dynamic> json) => EnumArrays(
        justSymbol: EnumArraysJustSymbolEnum.fromJson(json[r'just_symbol']),
        arrayEnum: EnumArraysArrayEnumEnum.listFromJson(json[r'array_enum']),
    );

  static List<EnumArrays> listFromJson(List json, {bool? growable,}) =>
    json.isNotEmpty
      ? json.map<EnumArrays>((i) => EnumArrays.fromJson(i as Map<String, dynamic>)).toList(growable: true == growable)
      : <EnumArrays>[];

  static Map<String, EnumArrays> mapFromJson(dynamic json) {
    final map = <String, EnumArrays>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) => map[key] = EnumArrays.fromJson(value));
    }
    return map;
  }

  // maps a json object with a list of EnumArrays-objects as value to a dart map
  static Map<String, List<EnumArrays>> mapListFromJson(dynamic json, {bool? growable,}) {
    final map = <String, List<EnumArrays>>{};
    if (json is Map && json.isNotEmpty) {
      json
        .cast<String, dynamic>()
        .forEach((key, dynamic value) {
          map[key] = EnumArrays.listFromJson(
            value,
            growable: growable,
          );
        });
    }
    return map;
  }
}


class EnumArraysJustSymbolEnum {
  /// Instantiate a new enum with the provided [value].
  const EnumArraysJustSymbolEnum._(this.value);

  /// The underlying value of this enum member.
  final String? value;

  @override
  String toString() => value ?? '';

  String? toJson() => value;

  static const greaterThanEqual = EnumArraysJustSymbolEnum._(r'>=');
  static const dollar = EnumArraysJustSymbolEnum._(r'$');

  /// List of all possible values in this [enum][EnumArraysJustSymbolEnum].
  static const values = <EnumArraysJustSymbolEnum>[
    greaterThanEqual,
    dollar,
  ];

  static EnumArraysJustSymbolEnum fromJson(dynamic value) =>
    EnumArraysJustSymbolEnumTypeTransformer().decode(value);

  static List<EnumArraysJustSymbolEnum> listFromJson(List json, {bool? growable,}) =>
    json.isNotEmpty
      ? json.map<EnumArraysJustSymbolEnum>((i) => EnumArraysJustSymbolEnum.fromJson(i as Map<String, dynamic>)).toList(growable: true == growable)
      : <EnumArraysJustSymbolEnum>[];
}

/// Transformation class that can [encode] an instance of [EnumArraysJustSymbolEnum] to String,
/// and [decode] dynamic data back to [EnumArraysJustSymbolEnum].
class EnumArraysJustSymbolEnumTypeTransformer {
  factory EnumArraysJustSymbolEnumTypeTransformer() => _instance ??= const EnumArraysJustSymbolEnumTypeTransformer._();

  const EnumArraysJustSymbolEnumTypeTransformer._();

  String? encode(EnumArraysJustSymbolEnum data) => data.value;

  /// Decodes a [dynamic value][data] to a EnumArraysJustSymbolEnum.
  ///
  /// If the [dynamic value][data] cannot be decoded successfully, then an [UnimplementedError] is thrown.
  EnumArraysJustSymbolEnum decode(dynamic data) {
    if (data == r'>=') {
      return EnumArraysJustSymbolEnum.greaterThanEqual;
    }
    if (data == r'$') {
      return EnumArraysJustSymbolEnum.dollar;
    }
    throw ArgumentError('Unknown enum value to decode: $data');
  }

  /// Singleton [EnumArraysJustSymbolEnumTypeTransformer] instance.
  static EnumArraysJustSymbolEnumTypeTransformer? _instance;
}



class EnumArraysArrayEnumEnum {
  /// Instantiate a new enum with the provided [value].
  const EnumArraysArrayEnumEnum._(this.value);

  /// The underlying value of this enum member.
  final String? value;

  @override
  String toString() => value ?? '';

  String? toJson() => value;

  static const fish = EnumArraysArrayEnumEnum._(r'fish');
  static const crab = EnumArraysArrayEnumEnum._(r'crab');

  /// List of all possible values in this [enum][EnumArraysArrayEnumEnum].
  static const values = <EnumArraysArrayEnumEnum>[
    fish,
    crab,
  ];

  static EnumArraysArrayEnumEnum fromJson(dynamic value) =>
    EnumArraysArrayEnumEnumTypeTransformer().decode(value);

  static List<EnumArraysArrayEnumEnum> listFromJson(List json, {bool? growable,}) =>
    json.isNotEmpty
      ? json.map<EnumArraysArrayEnumEnum>((i) => EnumArraysArrayEnumEnum.fromJson(i as Map<String, dynamic>)).toList(growable: true == growable)
      : <EnumArraysArrayEnumEnum>[];
}

/// Transformation class that can [encode] an instance of [EnumArraysArrayEnumEnum] to String,
/// and [decode] dynamic data back to [EnumArraysArrayEnumEnum].
class EnumArraysArrayEnumEnumTypeTransformer {
  factory EnumArraysArrayEnumEnumTypeTransformer() => _instance ??= const EnumArraysArrayEnumEnumTypeTransformer._();

  const EnumArraysArrayEnumEnumTypeTransformer._();

  String? encode(EnumArraysArrayEnumEnum data) => data.value;

  /// Decodes a [dynamic value][data] to a EnumArraysArrayEnumEnum.
  ///
  /// If the [dynamic value][data] cannot be decoded successfully, then an [UnimplementedError] is thrown.
  EnumArraysArrayEnumEnum decode(dynamic data) {
    if (data == r'fish') {
      return EnumArraysArrayEnumEnum.fish;
    }
    if (data == r'crab') {
      return EnumArraysArrayEnumEnum.crab;
    }
    throw ArgumentError('Unknown enum value to decode: $data');
  }

  /// Singleton [EnumArraysArrayEnumEnumTypeTransformer] instance.
  static EnumArraysArrayEnumEnumTypeTransformer? _instance;
}


