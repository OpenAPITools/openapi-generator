//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.12

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;


class EnumClass {
  /// Instantiate a new enum with the provided [value].
  const EnumClass._(this.value);

  /// The underlying value of this enum member.
  final String? value;

  @override
  String toString() => value ?? '';

  String? toJson() => value;

  static const abc = EnumClass._(r'_abc');
  static const efg = EnumClass._(r'-efg');
  static const leftParenthesisXyzRightParenthesis = EnumClass._(r'(xyz)');

  /// List of all possible values in this [enum][EnumClass].
  static const values = <EnumClass>[
    abc,
    efg,
    leftParenthesisXyzRightParenthesis,
  ];

  static EnumClass fromJson(dynamic value) =>
    EnumClassTypeTransformer().decode(value);

  static List<EnumClass> listFromJson(List json, {bool? growable,}) =>
    json.isNotEmpty
      ? json.map<EnumClass>((i) => EnumClass.fromJson(i as Map<String, dynamic>)).toList(growable: true == growable)
      : <EnumClass>[];
}

/// Transformation class that can [encode] an instance of [EnumClass] to String,
/// and [decode] dynamic data back to [EnumClass].
class EnumClassTypeTransformer {
  factory EnumClassTypeTransformer() => _instance ??= const EnumClassTypeTransformer._();

  const EnumClassTypeTransformer._();

  String? encode(EnumClass data) => data.value;

  /// Decodes a [dynamic value][data] to a EnumClass.
  ///
  /// If the [dynamic value][data] cannot be decoded successfully, then an [UnimplementedError] is thrown.
  EnumClass decode(dynamic data) {
    if (data == r'_abc') {
      return EnumClass.abc;
    }
    if (data == r'-efg') {
      return EnumClass.efg;
    }
    if (data == r'(xyz)') {
      return EnumClass.leftParenthesisXyzRightParenthesis;
    }
    throw ArgumentError('Unknown enum value to decode: $data');

  }

  /// Singleton [EnumClassTypeTransformer] instance.
  static EnumClassTypeTransformer? _instance;
}

