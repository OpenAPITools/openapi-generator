//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.14

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;


class OuterEnumInteger {
  /// Instantiate a new enum with the provided [value].
  const OuterEnumInteger._(this.value);

  /// The underlying value of this enum member.
  final int? value;

  @override
  String toString() => value == null ? '' : value.toString();

  int? toJson() => value;

  static const number0 = OuterEnumInteger._(0);
  static const number1 = OuterEnumInteger._(1);
  static const number2 = OuterEnumInteger._(2);

  /// List of all possible values in this [enum][OuterEnumInteger].
  static const values = <OuterEnumInteger>[
    number0,
    number1,
    number2,
  ];

  static OuterEnumInteger fromJson(dynamic value) =>
    OuterEnumIntegerTypeTransformer().decode(value);

  static List<OuterEnumInteger> listFromJson(List json, {bool? growable,}) =>
    json.isNotEmpty
      ? json.map<OuterEnumInteger>((i) => OuterEnumInteger.fromJson(i as Map<String, dynamic>)).toList(growable: true == growable)
      : <OuterEnumInteger>[];
}

/// Transformation class that can [encode] an instance of [OuterEnumInteger] to int,
/// and [decode] dynamic data back to [OuterEnumInteger].
class OuterEnumIntegerTypeTransformer {
  factory OuterEnumIntegerTypeTransformer() => _instance ??= const OuterEnumIntegerTypeTransformer._();

  const OuterEnumIntegerTypeTransformer._();

  int? encode(OuterEnumInteger data) => data.value;

  /// Decodes a [dynamic value][data] to a OuterEnumInteger.
  ///
  /// If the [dynamic value][data] cannot be decoded successfully, then an [UnimplementedError] is thrown.
  OuterEnumInteger decode(dynamic data) {
    if (data == 0) {
      return OuterEnumInteger.number0;
    }
    if (data == 1) {
      return OuterEnumInteger.number1;
    }
    if (data == 2) {
      return OuterEnumInteger.number2;
    }
    throw ArgumentError('Unknown enum value to decode: $data');

  }

  /// Singleton [OuterEnumIntegerTypeTransformer] instance.
  static OuterEnumIntegerTypeTransformer? _instance;
}

