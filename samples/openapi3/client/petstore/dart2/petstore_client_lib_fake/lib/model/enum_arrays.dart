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
        justSymbol: _$enumDecode(_$EnumArraysJustSymbolEnum, json[r'just_symbol']),
        arrayEnum: _$enumsDecode(_$EnumArraysArrayEnumEnum, json[r'array_enum']),
    );

  static List<EnumArrays> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <EnumArrays>[]
      : json.map((v) => EnumArrays.fromJson(v)).toList(growable: true == growable);

  static Map<String, EnumArrays> mapFromJson(Map<String, dynamic> json) {
    final map = <String, EnumArrays>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) => map[key] = EnumArrays.fromJson(v));
    }
    return map;
  }

  // maps a json object with a list of EnumArrays-objects as value to a dart map
  static Map<String, List<EnumArrays>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<EnumArrays>>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) {
        map[key] = EnumArrays.listFromJson(v, emptyIsNull: emptyIsNull, growable: growable);
      });
    }
    return map;
  }
}


enum EnumArraysJustSymbolEnum {
        greaterThanEqual,
        dollar,
}

const _$EnumArraysJustSymbolEnum = <EnumArraysJustSymbolEnum, dynamic>{
        EnumArraysJustSymbolEnum.greaterThanEqual: '>=',
        EnumArraysJustSymbolEnum.dollar: '$',
};



enum EnumArraysArrayEnumEnum {
        fish,
        crab,
}

const _$EnumArraysArrayEnumEnum = <EnumArraysArrayEnumEnum, dynamic>{
        EnumArraysArrayEnumEnum.fish: 'fish',
        EnumArraysArrayEnumEnum.crab: 'crab',
};


