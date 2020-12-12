//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;

class InlineObject2 {
  /// Returns a new [InlineObject2] instance.
  InlineObject2({
    this.enumFormStringArray = const [],
    this.enumFormString = '-efg',
  });

  /// Form parameter enum test (string array)
  List<InlineObject2EnumFormStringArrayEnum> enumFormStringArray;

  /// Form parameter enum test (string)
  InlineObject2EnumFormStringEnum enumFormString;

  @override
  bool operator ==(Object other) => identical(this, other) || other is InlineObject2 &&
     other.enumFormStringArray == enumFormStringArray &&
     other.enumFormString == enumFormString;

  @override
  int get hashCode =>
    (enumFormStringArray == null ? 0 : enumFormStringArray.hashCode) +
    (enumFormString == null ? 0 : enumFormString.hashCode);

  @override
  String toString() => 'InlineObject2[enumFormStringArray=$enumFormStringArray, enumFormString=$enumFormString]';

  Map<String, dynamic> toJson() {
    final json = <String, dynamic>{};
    if (enumFormStringArray != null) {
      json[r'enum_form_string_array'] = enumFormStringArray;
    }
    if (enumFormString != null) {
      json[r'enum_form_string'] = enumFormString;
    }
    return json;
  }

  /// Returns a new [InlineObject2] instance and imports its values from
  /// [json] if it's non-null, null if [json] is null.
  static InlineObject2 fromJson(Map<String, dynamic> json) => json == null
    ? null
    : InlineObject2(
        enumFormStringArray: InlineObject2EnumFormStringArrayEnum.listFromJson(json[r'enum_form_string_array']),
        enumFormString: InlineObject2EnumFormStringEnum.fromJson(json[r'enum_form_string']),
    );

  static List<InlineObject2> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <InlineObject2>[]
      : json.map((v) => InlineObject2.fromJson(v)).toList(growable: true == growable);

  static Map<String, InlineObject2> mapFromJson(Map<String, dynamic> json) {
    final map = <String, InlineObject2>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) => map[key] = InlineObject2.fromJson(v));
    }
    return map;
  }

  // maps a json object with a list of InlineObject2-objects as value to a dart map
  static Map<String, List<InlineObject2>> mapListFromJson(Map<String, dynamic> json, {bool emptyIsNull, bool growable,}) {
    final map = <String, List<InlineObject2>>{};
    if (json != null && json.isNotEmpty) {
      json.forEach((String key, dynamic v) {
        map[key] = InlineObject2.listFromJson(v, emptyIsNull: emptyIsNull, growable: growable);
      });
    }
    return map;
  }
}


class InlineObject2EnumFormStringArrayEnum {
  /// Instantiate a new enum with the provided [value].
  const InlineObject2EnumFormStringArrayEnum._(this.value);

  /// The underlying value of this enum member.
  final String value;

  @override
  bool operator ==(Object other) => identical(this, other) ||
      other is InlineObject2EnumFormStringArrayEnum && other.value == value;

  @override
  int get hashCode => toString().hashCode;

  @override
  String toString() => value;

  String toJson() => value;

  static const greaterThan = InlineObject2EnumFormStringArrayEnum._(r'>');
  static const dollar = InlineObject2EnumFormStringArrayEnum._(r'$');

  /// List of all possible values in this [enum][InlineObject2EnumFormStringArrayEnum].
  static const values = <InlineObject2EnumFormStringArrayEnum>[
    greaterThan,
    dollar,
  ];

  static InlineObject2EnumFormStringArrayEnum fromJson(dynamic value) =>
    InlineObject2EnumFormStringArrayEnumTypeTransformer().decode(value);

  static List<InlineObject2EnumFormStringArrayEnum> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <InlineObject2EnumFormStringArrayEnum>[]
      : json
          .map((value) => InlineObject2EnumFormStringArrayEnum.fromJson(value))
          .toList(growable: true == growable);
}

/// Transformation class that can [encode] an instance of [InlineObject2EnumFormStringArrayEnum] to String,
/// and [decode] dynamic data back to [InlineObject2EnumFormStringArrayEnum].
class InlineObject2EnumFormStringArrayEnumTypeTransformer {
  const InlineObject2EnumFormStringArrayEnumTypeTransformer._();

  factory InlineObject2EnumFormStringArrayEnumTypeTransformer() => _instance ??= InlineObject2EnumFormStringArrayEnumTypeTransformer._();

  String encode(InlineObject2EnumFormStringArrayEnum data) => data.value;

  /// Decodes a [dynamic value][data] to a InlineObject2EnumFormStringArrayEnum.
  ///
  /// If [allowNull] is true and the [dynamic value][data] cannot be decoded successfully,
  /// then null is returned. However, if [allowNull] is false and the [dynamic value][data]
  /// cannot be decoded successfully, then an [UnimplementedError] is thrown.
  ///
  /// The [allowNull] is very handy when an API changes and a new enum value is added or removed,
  /// and users are still using an old app with the old code.
  InlineObject2EnumFormStringArrayEnum decode(dynamic data, {bool allowNull}) {
    switch (data) {
      case r'>': return InlineObject2EnumFormStringArrayEnum.greaterThan;
      case r'$': return InlineObject2EnumFormStringArrayEnum.dollar;
      default:
        if (allowNull == false) {
          throw ArgumentError('Unknown enum value to decode: $data');
        }
    }
    return null;
  }

  /// Singleton [InlineObject2EnumFormStringArrayEnumTypeTransformer] instance.
  static InlineObject2EnumFormStringArrayEnumTypeTransformer _instance;
}

/// Form parameter enum test (string)
class InlineObject2EnumFormStringEnum {
  /// Instantiate a new enum with the provided [value].
  const InlineObject2EnumFormStringEnum._(this.value);

  /// The underlying value of this enum member.
  final String value;

  @override
  bool operator ==(Object other) => identical(this, other) ||
      other is InlineObject2EnumFormStringEnum && other.value == value;

  @override
  int get hashCode => toString().hashCode;

  @override
  String toString() => value;

  String toJson() => value;

  static const abc = InlineObject2EnumFormStringEnum._(r'_abc');
  static const efg = InlineObject2EnumFormStringEnum._(r'-efg');
  static const leftParenthesisXyzRightParenthesis = InlineObject2EnumFormStringEnum._(r'(xyz)');

  /// List of all possible values in this [enum][InlineObject2EnumFormStringEnum].
  static const values = <InlineObject2EnumFormStringEnum>[
    abc,
    efg,
    leftParenthesisXyzRightParenthesis,
  ];

  static InlineObject2EnumFormStringEnum fromJson(dynamic value) =>
    InlineObject2EnumFormStringEnumTypeTransformer().decode(value);

  static List<InlineObject2EnumFormStringEnum> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <InlineObject2EnumFormStringEnum>[]
      : json
          .map((value) => InlineObject2EnumFormStringEnum.fromJson(value))
          .toList(growable: true == growable);
}

/// Transformation class that can [encode] an instance of [InlineObject2EnumFormStringEnum] to String,
/// and [decode] dynamic data back to [InlineObject2EnumFormStringEnum].
class InlineObject2EnumFormStringEnumTypeTransformer {
  const InlineObject2EnumFormStringEnumTypeTransformer._();

  factory InlineObject2EnumFormStringEnumTypeTransformer() => _instance ??= InlineObject2EnumFormStringEnumTypeTransformer._();

  String encode(InlineObject2EnumFormStringEnum data) => data.value;

  /// Decodes a [dynamic value][data] to a InlineObject2EnumFormStringEnum.
  ///
  /// If [allowNull] is true and the [dynamic value][data] cannot be decoded successfully,
  /// then null is returned. However, if [allowNull] is false and the [dynamic value][data]
  /// cannot be decoded successfully, then an [UnimplementedError] is thrown.
  ///
  /// The [allowNull] is very handy when an API changes and a new enum value is added or removed,
  /// and users are still using an old app with the old code.
  InlineObject2EnumFormStringEnum decode(dynamic data, {bool allowNull}) {
    switch (data) {
      case r'_abc': return InlineObject2EnumFormStringEnum.abc;
      case r'-efg': return InlineObject2EnumFormStringEnum.efg;
      case r'(xyz)': return InlineObject2EnumFormStringEnum.leftParenthesisXyzRightParenthesis;
      default:
        if (allowNull == false) {
          throw ArgumentError('Unknown enum value to decode: $data');
        }
    }
    return null;
  }

  /// Singleton [InlineObject2EnumFormStringEnumTypeTransformer] instance.
  static InlineObject2EnumFormStringEnumTypeTransformer _instance;
}

