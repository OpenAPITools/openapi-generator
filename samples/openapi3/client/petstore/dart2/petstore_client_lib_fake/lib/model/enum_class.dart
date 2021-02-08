//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;


class EnumClass {
  /// Instantiate a new enum with the provided [value].
  const EnumClass._(this.value);

  /// The underlying value of this enum member.
  final String value;

  @override
  String toString() => value;

  String toJson() => value;

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

  static List<EnumClass> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <EnumClass>[]
      : json
          .map((value) => EnumClass.fromJson(value))
          .toList(growable: true == growable);
}

/// Transformation class that can [encode] an instance of [EnumClass] to String,
/// and [decode] dynamic data back to [EnumClass].
class EnumClassTypeTransformer {
  const EnumClassTypeTransformer._();

  factory EnumClassTypeTransformer() => _instance ??= EnumClassTypeTransformer._();

  String encode(EnumClass data) => data.value;

  /// Decodes a [dynamic value][data] to a EnumClass.
  ///
  /// If [allowNull] is true and the [dynamic value][data] cannot be decoded successfully,
  /// then null is returned. However, if [allowNull] is false and the [dynamic value][data]
  /// cannot be decoded successfully, then an [UnimplementedError] is thrown.
  ///
  /// The [allowNull] is very handy when an API changes and a new enum value is added or removed,
  /// and users are still using an old app with the old code.
  EnumClass decode(dynamic data, {bool allowNull}) {
    switch (data) {
      case r'_abc': return EnumClass.abc;
      case r'-efg': return EnumClass.efg;
      case r'(xyz)': return EnumClass.leftParenthesisXyzRightParenthesis;
      default:
        if (allowNull == false) {
          throw ArgumentError('Unknown enum value to decode: $data');
        }
    }
    return null;
  }

  /// Singleton [EnumClassTypeTransformer] instance.
  static EnumClassTypeTransformer _instance;
}
