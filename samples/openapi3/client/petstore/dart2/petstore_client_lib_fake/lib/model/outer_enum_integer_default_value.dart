//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.0

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;


class OuterEnumIntegerDefaultValue {
  /// Instantiate a new enum with the provided [value].
  const OuterEnumIntegerDefaultValue._(this.value);

  /// The underlying value of this enum member.
  final int value;

  @override
  String toString() => value.toString();

  int toJson() => value;

  static const number0 = OuterEnumIntegerDefaultValue._(0);
  static const number1 = OuterEnumIntegerDefaultValue._(1);
  static const number2 = OuterEnumIntegerDefaultValue._(2);

  /// List of all possible values in this [enum][OuterEnumIntegerDefaultValue].
  static const values = <OuterEnumIntegerDefaultValue>[
    number0,
    number1,
    number2,
  ];

  static OuterEnumIntegerDefaultValue fromJson(dynamic value) =>
    OuterEnumIntegerDefaultValueTypeTransformer().decode(value);

  static List<OuterEnumIntegerDefaultValue> listFromJson(List<dynamic> json, {bool emptyIsNull, bool growable,}) =>
    json == null || json.isEmpty
      ? true == emptyIsNull ? null : <OuterEnumIntegerDefaultValue>[]
      : json
          .map((value) => OuterEnumIntegerDefaultValue.fromJson(value))
          .toList(growable: true == growable);
}

/// Transformation class that can [encode] an instance of [OuterEnumIntegerDefaultValue] to int,
/// and [decode] dynamic data back to [OuterEnumIntegerDefaultValue].
class OuterEnumIntegerDefaultValueTypeTransformer {
  const OuterEnumIntegerDefaultValueTypeTransformer._();

  factory OuterEnumIntegerDefaultValueTypeTransformer() => _instance ??= OuterEnumIntegerDefaultValueTypeTransformer._();

  int encode(OuterEnumIntegerDefaultValue data) => data.value;

  /// Decodes a [dynamic value][data] to a OuterEnumIntegerDefaultValue.
  ///
  /// If [allowNull] is true and the [dynamic value][data] cannot be decoded successfully,
  /// then null is returned. However, if [allowNull] is false and the [dynamic value][data]
  /// cannot be decoded successfully, then an [UnimplementedError] is thrown.
  ///
  /// The [allowNull] is very handy when an API changes and a new enum value is added or removed,
  /// and users are still using an old app with the old code.
  OuterEnumIntegerDefaultValue decode(dynamic data, {bool allowNull}) {
    switch (data) {
      case 0: return OuterEnumIntegerDefaultValue.number0;
      case 1: return OuterEnumIntegerDefaultValue.number1;
      case 2: return OuterEnumIntegerDefaultValue.number2;
      default:
        if (allowNull == false) {
          throw ArgumentError('Unknown enum value to decode: $data');
        }
    }
    return null;
  }

  /// Singleton [OuterEnumIntegerDefaultValueTypeTransformer] instance.
  static OuterEnumIntegerDefaultValueTypeTransformer _instance;
}
