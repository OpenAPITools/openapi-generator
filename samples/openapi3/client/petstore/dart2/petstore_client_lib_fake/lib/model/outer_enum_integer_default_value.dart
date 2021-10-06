//
// AUTO-GENERATED FILE, DO NOT MODIFY!
//
// @dart=2.14

// ignore_for_file: unused_element, unused_import
// ignore_for_file: always_put_required_named_parameters_first
// ignore_for_file: constant_identifier_names
// ignore_for_file: lines_longer_than_80_chars

part of openapi.api;


class OuterEnumIntegerDefaultValue {
  /// Instantiate a new enum with the provided [value].
  const OuterEnumIntegerDefaultValue._(this.value);

  /// The underlying value of this enum member.
  final int? value;

  @override
  String toString() => value == null ? '' : value.toString();

  int? toJson() => value;

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

  static List<OuterEnumIntegerDefaultValue> listFromJson(List json, {bool? growable,}) =>
    json.isNotEmpty
      ? json.map<OuterEnumIntegerDefaultValue>((i) => OuterEnumIntegerDefaultValue.fromJson(i as Map<String, dynamic>)).toList(growable: true == growable)
      : <OuterEnumIntegerDefaultValue>[];
}

/// Transformation class that can [encode] an instance of [OuterEnumIntegerDefaultValue] to int,
/// and [decode] dynamic data back to [OuterEnumIntegerDefaultValue].
class OuterEnumIntegerDefaultValueTypeTransformer {
  factory OuterEnumIntegerDefaultValueTypeTransformer() => _instance ??= const OuterEnumIntegerDefaultValueTypeTransformer._();

  const OuterEnumIntegerDefaultValueTypeTransformer._();

  int? encode(OuterEnumIntegerDefaultValue data) => data.value;

  /// Decodes a [dynamic value][data] to a OuterEnumIntegerDefaultValue.
  ///
  /// If the [dynamic value][data] cannot be decoded successfully, then an [UnimplementedError] is thrown.
  OuterEnumIntegerDefaultValue decode(dynamic data) {
    if (data == 0) {
      return OuterEnumIntegerDefaultValue.number0;
    }
    if (data == 1) {
      return OuterEnumIntegerDefaultValue.number1;
    }
    if (data == 2) {
      return OuterEnumIntegerDefaultValue.number2;
    }
    throw ArgumentError('Unknown enum value to decode: $data');

  }

  /// Singleton [OuterEnumIntegerDefaultValueTypeTransformer] instance.
  static OuterEnumIntegerDefaultValueTypeTransformer? _instance;
}

